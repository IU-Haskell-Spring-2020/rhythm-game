{-# LANGUAGE OverloadedStrings #-}

-- | This is a core engine module, and should probably not used directly.
-- | There is also `Framework.hs` nearby, which exposes a more high-level API.

module Engine where

import Control.Monad
import Foreign.C.Types
import SDL (($=))
import Data.Default.Class (def)
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Image
import qualified SDL.Mixer

-- =============================================================================
-- Output interface {{{1

-- | Key press event that is going to be sent to the application
data KeyPress
  = KeyLeft | KeyRight | KeyUp | KeyDown

-- | Output action from state changes
data Action
  = RestartMusic
  | Print String
  | CombinedAction Action Action
  | NoAction

-- | Output picture drawn on the screen.
data Picture
  = Draw Float (Float, Float) Int     -- ^ Draw a picture by its index at coords
  | CombinedPicture Picture Picture   -- ^ Two output pictures combined
  | Blank                             -- ^ No-op picture

class Combinable a where
  combine :: a -> a -> a

instance Combinable Action where
  combine a NoAction = a
  combine NoAction a = a
  combine a1 a2 = CombinedAction a1 a2

instance Combinable Picture where
  combine p Blank = p
  combine Blank p = p
  combine p1 p2 = CombinedPicture p1 p2

-- | Transated picture
translated :: (Float, Float) -> Picture -> Picture
translated t (CombinedPicture o1 o2) = CombinedPicture (translated t o1) (translated t o2)
translated (dx, dy) (Draw s (x, y) i) = Draw s (x + dx, y + dy) i
translated _ x = x

-- | Scaled picture
scaled :: Float -> Picture -> Picture
scaled s (CombinedPicture o1 o2) = CombinedPicture (scaled s o1) (scaled s o2)
scaled s (Draw s' p i) = Draw (s * s') p i
scaled _ x = x

-- =============================================================================
-- Generic main function {{{1

targetFPS = 60

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

runSDL
  -- initialization
  :: world                                  -- ^ Initial world state
  -> [String]                               -- ^ Picture paths
  -> String                                 -- ^ Root music path
  -- loop
  -> (KeyPress -> world -> world)           -- ^ Event handler
  -> (Float -> world -> (world, Action))    -- ^ World updater
  -> (world -> Picture)                     -- ^ World painter
  -- output
  -> IO ()

runSDL
  initialWorld
  imagePaths
  musicPath
  handleEvent
  updateWorld
  drawWorld
    = do
      SDL.initialize [SDL.InitVideo, SDL.InitAudio]
      SDL.Mixer.initialize [SDL.Mixer.InitOGG]
      SDL.Mixer.openAudio def 256

      music <- SDL.Mixer.load musicPath :: IO SDL.Mixer.Music

      window <-
        SDL.createWindow
          "SDL / OpenGL Example"
          SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 screenWidth screenHeight,
                             SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}
      SDL.showWindow window

      _ <- SDL.glCreateContext window
      windowSurface <- SDL.getWindowSurface window
      images <- loadImages imagePaths

      SDL.Mixer.playMusic SDL.Mixer.Once music

      let loop lastFrame world = do
            time <- SDL.time
            let dt = time - lastFrame

            let (updatedWorld, action) = updateWorld dt world

            case action of
              (Print s) -> print s
              _         -> return ()

            events <- SDL.pollEvents
            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
            let keyPresses = mapEvents events

            let handledWorld = handleEvents keyPresses handleEvent updatedWorld

            SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
            draw images (drawWorld handledWorld) windowSurface

            SDL.updateWindowSurface window
            SDL.glSwapWindow window

            unless quit (loop time handledWorld)

      time <- SDL.time
      loop time initialWorld

      SDL.destroyWindow window
      SDL.quit

handleEvents :: [KeyPress] -> (KeyPress -> world -> world) -> world -> world
handleEvents [] _ world = world
handleEvents (event:events) handler world
  = handleEvents events handler (handler event world)

mapEvents :: [SDL.Event] -> [KeyPress]
mapEvents [] = []
mapEvents (event:events) = outputEvents
  where
    payload = SDL.eventPayload event
    tailEvents = mapEvents events
    maybeTargetEvent
      = case payload of
          SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym code _ _))
            -> mapCode code
          _ -> Nothing
    outputEvents
      = case maybeTargetEvent of
          Just event -> event:tailEvents
          Nothing -> tailEvents


mapCode :: SDL.Scancode -> Maybe KeyPress
mapCode SDL.ScancodeUp = Just KeyUp
mapCode SDL.ScancodeDown = Just KeyDown
mapCode SDL.ScancodeLeft = Just KeyLeft
mapCode SDL.ScancodeRight = Just KeyRight
mapCode _ = Nothing

-- | Load images from paths into an array.
loadImages :: [String] -> IO [SDL.Surface]
loadImages paths
  = foldM executeAndAppend [] (map SDL.Image.load paths)
    where
      executeAndAppend list action = do
        output <- action
        return (output:list)

draw :: [SDL.Surface] -> Picture -> SDL.Surface -> IO ()
draw textures Blank _ = return ()
draw textures (CombinedPicture pic1 pic2) target = do
  draw textures pic1 target
  draw textures pic2 target
draw textures (Draw s (x, y) i) target = do
  let texture = textures !! i
  (SDL.V2 w h) <- SDL.surfaceDimensions texture

  let targetX = round x :: CInt
  let targetY = round y :: CInt
  let targetW = round (fromIntegral w * s)
  let targetH = round (fromIntegral h * s)

  SDL.surfaceBlitScaled
    (textures !! i)
    Nothing
    target
    (Just $ SDL.Rectangle
      (SDL.P $ SDL.V2 targetX targetY)
      (SDL.V2 targetW targetH))

-- vim: set ts=2 sw=2 fdm=marker:
