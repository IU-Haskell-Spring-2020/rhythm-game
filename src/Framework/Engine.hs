{-# LANGUAGE OverloadedStrings #-}

module Framework.Engine where

import System.Random
import Control.Monad
import Foreign.C.Types
import SDL (($=))
import Data.Default.Class (def)
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Image
import qualified SDL.Mixer

import Math

-- =============================================================================
-- Output interface {{{1

-- | Key press event that is going to be sent to the application
data KeyPress
  = KeyLeft | KeyRight | KeyUp | KeyDown

-- | Output action from state changes
data Action
  = RestartMusic
  | PlayOooEffect
  | Print String
  | CombinedAction Action Action
  | NoAction

-- | Output picture drawn on the screen.
data Picture target
  = Draw Float (Float, Float) target     -- ^ Draw a picture by its index at coords
  | CombinedPicture (Picture target) (Picture target)
                                         -- ^ Two output pictures combined
  | Blank                                -- ^ No-op picture

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
      -- TODO: hardcode
      sounds <- executeAll [
          SDL.Mixer.load "resources/hurt1.ogg" :: IO SDL.Mixer.Chunk,
          SDL.Mixer.load "resources/hurt2.ogg" :: IO SDL.Mixer.Chunk,
          SDL.Mixer.load "resources/hurt3.ogg" :: IO SDL.Mixer.Chunk
        ]

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

            randomSoundIdx <- getStdRandom $ randomR (0, length sounds - 1)
            let randomSound = sounds !! randomSoundIdx

            case action of
              (Print s)     -> print s
              PlayOooEffect -> SDL.Mixer.play randomSound
              _             -> return ()

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

mapCode SDL.ScancodeLeft = Just KeyLeft
mapCode SDL.ScancodeA = Just KeyLeft

mapCode SDL.ScancodeRight = Just KeyRight
mapCode SDL.ScancodeD = Just KeyRight

mapCode _ = Nothing

-- | Load images from paths into an array.
loadImages :: [String] -> IO [SDL.Surface]
loadImages = executeAll . map SDL.Image.load

executeAndAppend :: [a] -> IO a -> IO [a]
executeAndAppend list action
  = do
      output <- action
      return (output:list)

executeAll :: [IO a] -> IO [a]
executeAll = foldM executeAndAppend []

draw :: [SDL.Surface] -> EnginePicture -> SDL.Surface -> IO ()
draw textures Blank _ = return ()
draw textures (CombinedPicture pic1 pic2) target = do
  draw textures pic1 target
  draw textures pic2 target
draw textures (Draw s (x, y) i) target = do
  let texture = textures !! i
  (SDL.V2 w h) <- SDL.surfaceDimensions texture

  let targetW = round (fromIntegral w * s)
  let targetH = round (fromIntegral h * s)
  let targetX = (round x - targetW `div` 2) :: CInt
  let targetY = (round y - targetH `div` 2) :: CInt

  SDL.surfaceBlitScaled
    (textures !! i)
    Nothing
    target
    (Just $ SDL.Rectangle
      (SDL.P $ SDL.V2 targetX targetY)
      (SDL.V2 targetW targetH))

-- vim: set ts=2 sw=2 fdm=marker:
