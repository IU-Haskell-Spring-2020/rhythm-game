module Render.ItemTile where

import Framework.Types
import Grid
import Types.Items

renderItemTile :: ItemTile -> GridPicture
renderItemTile me = pic
  where
    pic = translated position $ scaled scale $ texture name
    name = "item_" ++ itemTileName me
    scale = 0.8
    position = itemTilePosition me
