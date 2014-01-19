module Tile where

data Tile = Empty | X | O

instance Show Tile where
    show (Empty) = "."
    show (X) = "X"
    show (O) = "O"

instance Eq Tile where
    X == X = True
    Empty == Empty = True
    O == O = True
    _ == _  = False

-------------------------------------------------------------------------------
-- toTile c - Maps Char to corresponing Tile
-------------------------------------------------------------------------------
toTile '.' = Empty
toTile 'X' = X
toTile 'O' = O
toTile p = error [p]

-------------------------------------------------------------------------------
-- opposite t - Returns the opposite of a Tile
-------------------------------------------------------------------------------
opposite X = O
opposite O = X
opposite Empty = Empty
