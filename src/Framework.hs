{-# LANGUAGE ExistentialQuantification #-}

-- | A small actor framework to simplify state management.

module Framework (
  Actor(..),
  AnyActor(..),
  makeActor,
  runActors
) where

import Event
import Engine (Action(..), runSDL, KeyPress, Picture(..), combine)

-- | An actor in the game world.
class Actor a where
  -- | Texture name
  texturePath :: a -> String
  -- | Draw function, but without the actual texture id
  drawFunc :: a -> (Float, (Float, Float))
  -- | Event handler
  handleEventFunc :: a -> Event -> a
  handleEventFunc a _ = a
  -- | Updater
  updateFunc :: a -> Float -> (a, Event)
  updateFunc a _ = (a, NoEvent)

-- | Following two definitions are probably a hack.
-- | They enable OOP-like dynamic dispatch.
data AnyActor = forall a. Actor a => AnyActor a

instance Actor AnyActor where
  texturePath (AnyActor a) = texturePath a
  drawFunc (AnyActor a) = drawFunc a
  handleEventFunc (AnyActor a) = AnyActor . handleEventFunc a
  updateFunc (AnyActor a) dt = (AnyActor actor, action)
    where
      (actor, action) = updateFunc a dt

-- | This is just for readability.
makeActor :: Actor a => a -> AnyActor
makeActor = AnyActor

-- =============================================================================
-- Public API {{{1

-- -- | Update the state of an actor.
-- updateState :: s -> Actor s -> Actor s
-- updateState newState (Actor state a b c d) = Actor newState a b c d

-- | Run actor interaction in a loop.
runActors :: [AnyActor] -> String -> IO ()
runActors actors musicPath
  = runSDL
      (World actors NoEvent)
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
data World = World [AnyActor] Event

-- | Handle a specified event for a whole world
handleEvent :: KeyPress -> World -> World
handleEvent event (World actors events)
  = World actors (combine (KeyPress event) events)

-- | Update the world
update :: Float -> World -> (World, Action)
update dt world = publishEvents $ updateActors dt world

-- | Update actors in the game world, updating the global event list.
updateActors :: Float -> World -> World
updateActors _ (World [] events) = World [] events
updateActors dt (World (actor:actors) events)
  = World (newActor:newActors) (combine newEvent newEvents)
    where
      (newActor, newEvent) = updateActor actor dt
      (World newActors newEvents) = updateActors dt (World actors events)

-- | Publish events which were gathered during the `updateActors` phase.
publishEvents :: World -> (World, Action)
publishEvents (World actors NoEvent) = (World actors NoEvent, NoAction)
publishEvents (World actors (CombinedEvent e1 e2)) = (newWorld, combinedActions)
  where
    (w1, a1) = publishEvents (World actors e1)
    (World intermediateActors _) = w1
    (newWorld, a2) = publishEvents (World intermediateActors e2)
    combinedActions = combine a1 a2
publishEvents (World actors event)
  = (World (map handle actors) NoEvent, NoAction)
    where
      handle actor = handleEventFunc actor event

-- | Update one actor
updateActor :: AnyActor -> Float -> (AnyActor, Event)
updateActor = updateFunc

-- | Draw the world
draw :: (String -> Int) -> World -> Picture
draw textureId (World actors _)
  = foldr (combine . drawActor textureId) Blank actors

-- | Draw one actor
drawActor :: (String -> Int) -> AnyActor -> Picture
drawActor textureId actor = Draw scale position (textureId $ texturePath actor)
  where
    (scale, position) = drawFunc actor

-- | Get picture names from a list of actors.
-- | Also returns a function, mapping a picture path to a texture number.
pictureNames :: [AnyActor] -> ([String], String -> Int)
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
