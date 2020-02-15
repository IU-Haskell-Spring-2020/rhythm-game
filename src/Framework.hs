-- | A small actor framework to simplify state management.

module Framework (
  Actor(..),
  updateState,
  runActors
) where

import Engine (Action(..), runSDL, KeyPress, Picture(..), combine)

-- =============================================================================
-- Public API {{{1

-- | An actor in the game world.
-- | Has his own position, picture path, some handlers.
data Actor state = Actor {
  -- | Internal state
  state :: state,
  -- | Texture name
  texturePath :: String,
  -- | Draw function, but without the actual texture id
  drawFunc :: Actor state -> (Float, (Float, Float)),
  -- | Event handler
  handleEventFunc
    :: KeyPress
    -> Actor state
    -> Actor state,
  -- | Updater
  updateFunc
    :: Float
    -> Actor state
    -> (Actor state, Action)
  }

-- | Update the state of an actor.
updateState :: s -> Actor s -> Actor s
updateState newState (Actor state a b c d) = Actor newState a b c d

-- | Run actor interaction in a loop.
runActors :: [Actor a] -> String -> IO ()
runActors actors musicPath
  = runSDL
      (World actors)
      texturePaths
      musicPath
      handleEvent
      update
      (draw textureId)
        where
          (texturePaths, textureId) = pictureNames actors

-- =============================================================================
-- Actor state management and stuff {{{1

-- | Whole world type
data World a = World [Actor a]

-- | Handle a specified event for a whole world
handleEvent :: KeyPress -> World a -> World a
handleEvent event (World actors)
  = World (map (handleEventActor event) actors)

-- | Handle a specified event for an actor
handleEventActor :: KeyPress -> Actor a -> Actor a
handleEventActor event actor = internalHandleEvent event actor
  where internalHandleEvent = handleEventFunc actor

-- | Update the world
-- | TODO: this is really ugly
update :: Float -> World a -> (World a, Action)
update _ (World []) = (World [], NoAction)
update dt (World (actor:actors))
  = (World (newActor:newActors), combine newAction actions)
    where
      (newActor, newAction) = updateActor dt actor
      (newWorld, actions) = update dt (World actors)
      (World newActors) = newWorld

-- | Update one actor
updateActor :: Float -> Actor a -> (Actor a, Action)
updateActor dt actor = internalUpdateActor dt actor
  where
    internalUpdateActor = updateFunc actor

-- | Draw the world
draw :: (String -> Int) -> World a -> Picture
draw textureId (World actors)
  = foldr (combine . drawActor textureId) Blank actors

-- | Draw one actor
drawActor :: (String -> Int) -> Actor a -> Picture
drawActor textureId actor = Draw scale position (textureId $ texturePath actor)
  where
    (scale, position) = drawFunc actor actor

-- | Get picture names from a list of actors.
-- | Also returns a function, mapping a picture path to a texture number.
pictureNames :: [Actor a] -> ([String], String -> Int)
pictureNames [] = ([], const (-1))
pictureNames (actor:actors)
  | textureId name == -1 = (name:paths, wrappedTextureId)
  | otherwise            = (paths, textureId)
  where
    name = texturePath actor
    (paths, textureId) = pictureNames actors
    nextTextureId = length paths
    wrappedTextureId path
      | path == name = nextTextureId
      | otherwise    = textureId path

-- vim: set ts=2 sw=2 fdm=marker:
