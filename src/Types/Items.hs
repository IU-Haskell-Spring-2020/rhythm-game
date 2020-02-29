-- | Items.hs
--
-- Implements an item container, which can contain ... items :)
--
-- Basically, just a set of unique strings, because doing otherwise seems a bit
-- like an overgeneralization.
--
-- All logic regarding how these items affect the entities should be implemented
-- inside the target entity handling code.
--
-- Also, implements an ItemTile, which is simply used to draw items on the floor
-- and in the HUD.

module Types.Items where

-- | Item container type.
type Items = [String]

-- | Add a new item to the list.
addItem :: String -> Items -> Items
addItem item items
  | item `elem` items = items
  | otherwise         = item : items

-- | Remove an existing item from the list
removeItem :: String -> Items -> Items
removeItem item = filter (/= item)

-- | Simple item tile.
data ItemTile = ItemTile {
  itemTilePosition :: (Float, Float),
  itemTileName :: String
}

-- | Convenience function for initializing the item tile.
initItemTile :: (Float, Float) -> String -> ItemTile
initItemTile position name = ItemTile {
  itemTilePosition = position,
  itemTileName = name
}

-- | Map initializer.
itemTiles :: [ItemTile]
itemTiles = [
    ItemTile (1, 1) "beatSlowdown"
  ]
