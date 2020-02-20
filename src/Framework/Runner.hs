module Framework.Runner (
  runWorld
) where

import Engine (runSDL, EnginePicture, Picture(..))
import Framework.Types
import qualified World

-- =============================================================================
-- Public API {{{1

-- | Run world interaction in a loop.
runWorld :: IO ()
runWorld
  = runSDL
      World.init
      texturePaths
      "resources/scattered-and-lost.ogg"
      World.handleEvent
      World.update
      (convertPictures textureId . World.draw)
        where
          (texturePaths, textureId) = parseTextureNames World.textureNames

-- =============================================================================
-- Helper functions {{{1

-- | Convert all string pictures into int pictures.
convertPictures :: (String -> Maybe Int) -> StringPicture -> EnginePicture
convertPictures mapper Blank = Blank
convertPictures mapper (CombinedPicture p1 p2)
  = CombinedPicture (convertPictures mapper p1) (convertPictures mapper p2)
convertPictures mapper (Draw s p i)
  = case mapper i of
      Just x -> Draw s p x
      Nothing -> Blank

-- | Texture paths helper.
-- | Receives a list of pairs - mapping from texture name to its path,
-- | and returns a path list together with a mapping function from name to index.
parseTextureNames :: [(String, String)] -> ([String], String -> Maybe Int)
parseTextureNames [] = ([], const Nothing)
parseTextureNames ((name, path):others)
  = (path:paths, newMapper)
    where
      (paths, mapper) = parseTextureNames others

      newMapper :: String -> Maybe Int
      newMapper textureName
        | textureName == name = Just (length paths)
        | otherwise           = mapper textureName


-- vim: set ts=2 sw=2 fdm=marker:
