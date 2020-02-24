{-# LANGUAGE OverloadedStrings #-}

module Engine where

import Control.Monad
import Foreign.C.Types
import SDL (($=))
import Data.Default.Class (def)
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Image
import qualified SDL.Mixer

import Math
import qualified Box

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
data Picture target
  = Draw Box.Box target       -- ^ Draw a picture by its index inside the box
  | CombinedPicture (Picture target) (Picture target)
                              -- ^ Two output pictures combined
  | Blank                     -- ^ No-op picture

type EnginePicture = Picture Int

-- =============================================================================
-- Generic main function {{{1

targetFPS = 60

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 640)

runSDL
  -- initialization
  :: world                                  -- ^ Initial world state
  -> [String]                               -- ^ Picture paths
  -> String                                 -- ^ Root music path
  -- loop
  -> (KeyPress -> world -> world)           -- ^ Event handler
  -> (Float -> world -> (world, Action))    -- ^ World updater
  -> (world -> EnginePicture)                  -- ^ World painter
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
mapCode SDL.ScancodeW = Just KeyUp

mapCode SDL.ScancodeDown = Just KeyDown
mapCode SDL.ScancodeS = Just KeyDown
--
mapCode SDL.ScancodeLeft = Just KeyLeft
mapCode SDL.ScancodeA = Just KeyLeft

mapCode SDL.ScancodeRight = Just KeyRight
mapCode SDL.ScancodeD = Just KeyRight

mapCode _ = Nothing

-- | Load images from paths into an array.
loadImages :: [String] -> IO [SDL.Surface]
loadImages paths
  = foldM executeAndAppend [] (map SDL.Image.load paths)
    where
      executeAndAppend list action = do
        output <- action
        return (output:list)

draw :: [SDL.Surface] -> EnginePicture -> SDL.Surface -> IO ()
draw textures Blank _ = return ()
draw textures (CombinedPicture pic1 pic2) target = do
  draw textures pic1 target
  draw textures pic2 target
draw textures (Draw box i) target = do
  let texture = textures !! i
  (SDL.V2 w h) <- SDL.surfaceDimensions texture

  let (floatTargetW, floatTargetH) = Box.dimensions box
  let (floatTargetX, floatTargetY) =
        vSub (Box.position box) (vMul (floatTargetW, floatTargetH) 0.5)

  let targetX = round floatTargetX :: CInt
  let targetY = round floatTargetY :: CInt
  let targetW = round floatTargetW :: CInt
  let targetH = round floatTargetH :: CInt

  SDL.surfaceBlitScaled
    (textures !! i)
    Nothing
    target
    (Just $ SDL.Rectangle
      (SDL.P $ SDL.V2 targetX targetY)
      (SDL.V2 targetW targetH))

-- vim: set ts=2 sw=2 fdm=marker:
