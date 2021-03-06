module Render.ItemsHUD where

import Framework.Types
import Types.Items
import Render.ItemTile
import Grid

renderItemsHUD :: Items -> StringPicture
renderItemsHUD items = gridPicToWorldPic $ foldr combine Blank itemPictures
  where
    itemPictures = map renderItemTile itemTiles
    itemTiles = zipWith makeItemTile [0..] items
    makeItemTile offset = ItemTile (offset, 0)
