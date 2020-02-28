module Types.NPC where

import Types.SmoothPosition

data NPC = NPC {
  npcPosition :: SmoothPosition,
  npcSmth :: Int
}

initNpc :: NPC
initNpc = NPC {
  npcPosition = initSmoothPosition,
  npcSmth = 0
}

npcCurrentPosition :: NPC -> (Float, Float)
npcCurrentPosition npc = currentSmoothPosition (npcPosition npc)



