module WorldMap (
  WorldMap,
  WorldMap.init,
  textureNames,
  draw,
  update,
  handleEvent
) where

import Framework.Types
import Grid
import qualified RainbowFloor
import qualified Player

data WorldMap = WorldMap {
  floor :: RainbowFloor.RainbowFloor,
  player :: Player.Player
}

textureNames :: [(String, String)]
textureNames = RainbowFloor.textureNames

init :: WorldMap
init = WorldMap {
  WorldMap.floor = RainbowFloor.init,
  player = Player.init
}

draw :: WorldMap -> [StringPicture]
draw me = [
    RainbowFloor.draw (WorldMap.floor me)
  ] ++ map gridPicToWorldPic [
    Player.draw (player me)
  ]

update :: Float -> WorldMap -> WorldMap
update dt me = me {
  WorldMap.floor = RainbowFloor.update dt (WorldMap.floor me),
  player = Player.update dt (player me)
}

handleEvent :: KeyPress -> WorldMap -> WorldMap
handleEvent event me = me {
  player = Player.handleEvent event (player me)
}
