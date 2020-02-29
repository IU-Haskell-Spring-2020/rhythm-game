module Framework.Runner (
  runWorld
) where

import Framework.Engine (runSDL, EnginePicture, Picture(..))
import Framework.Types

import Types.World
import Render.World
import Update.World
import HandleEvent.World

-- =============================================================================
-- Public API {{{1

-- | Run world interaction in a loop.
runWorld :: IO ()
runWorld
  = runSDL
      initWorld
      texturePaths
      "resources/scattered-and-lost.ogg"
      handleEventWorld
      updateWorld
      (convertPictures textureId . renderWorld)
        where
          (texturePaths, textureId) = parseTextureNames [
            ("floor", "resources/floor.jpg"),
            ("fff", "resources/fff.jpg"),
            ("fff_red", "resources/fff_red.jpg"),
            ("clip", "resources/clip.jpg"),

            ("item_beatSlowdown", "resources/item_beatSlowdown.png")
            ]

-- =============================================================================
-- Helper functions {{{1

-- | Convert all string pictures into int pictures.
convertPictures :: (String -> Maybe Int) -> StringPicture -> EnginePicture
convertPictures mapper Blank = Blank
convertPictures mapper (CombinedPicture p1 p2)
  = CombinedPicture (convertPictures mapper p1) (convertPictures mapper p2)
convertPictures mapper (Draw scale pos i)
  = case mapper i of
      Just x -> Draw scale pos x
      Nothing -> Blank

-- | Texture paths helper.
-- | Receives a list of pairs - mapping from texture name to its path,
-- | and returns a path list together with a mapping function from name to index.
parseTextureNames :: [(String, String)] -> ([String], String -> Maybe Int)
parseTextureNames [] = ([], \name -> error ("wrong texture name: " ++ name))
parseTextureNames ((name, path):others)
  = (path:paths, newMapper)
    where
      (paths, mapper) = parseTextureNames others

      newMapper :: String -> Maybe Int
      newMapper textureName
        | textureName == name = Just (length paths)
        | otherwise           = mapper textureName


-- vim: set ts=2 sw=2 fdm=marker:
