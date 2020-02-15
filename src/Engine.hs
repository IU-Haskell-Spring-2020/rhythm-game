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

-- =============================================================================
-- Output interface {{{1

-- | Key press event that is going to be sent to the application
data KeyPress = KeyPress String

-- | Output action from state changes
data Action
  = RestartMusic
  | NoAction

-- | Output picture drawn on the screen.
data Picture
  = Draw (Float, Float) Int       -- ^ Draw a picture by its index at coords
  | Combined Picture Picture      -- ^ Two output pictures combined
  | Blank                         -- ^ No-op picture

-- | Transated picture
translated :: (Float, Float) -> Picture -> Picture
translated t (Combined o1 o2) = Combined (translated t o1) (translated t o2)
translated (dx, dy) (Draw (x, y) i) = Draw (x + dx, y + dy) i

-- =============================================================================
-- Generic main function {{{1

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

      let loop world = do
            events <- SDL.pollEvents
            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

            GL.clear [GL.ColorBuffer]
            draw images (drawWorld world) windowSurface
            SDL.updateWindowSurface window
            SDL.glSwapWindow window

            unless quit (loop world)

      loop initialWorld

      SDL.destroyWindow window
      SDL.quit

-- | Load images from paths into an array.
loadImages :: [String] -> IO [SDL.Surface]
loadImages paths
  = foldM executeAndAppend [] (map SDL.Image.load paths)
    where
      executeAndAppend list action = do
        output <- action
        return (output:list)

draw :: [SDL.Surface] -> Picture -> SDL.Surface -> IO (Maybe (SDL.Rectangle CInt))
draw textures Blank _ = return Nothing
draw textures (Combined pic1 pic2) target = do
  draw textures pic1 target
  draw textures pic2 target
draw textures (Draw (x, y) i) target
  = SDL.surfaceBlit
      (textures !! i)
      Nothing
      target
      (Just $ SDL.P $ SDL.V2 (round x :: CInt) (round y))

-- vim: set ts=2 sw=2 fdm=marker:
