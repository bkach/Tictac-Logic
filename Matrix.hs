module Matrix where

import Tile
import Data.Sequence
import Data.List
import Data.Foldable

-------------------------------------------------------------------------------
-- inBounds board coord - Checks if coordinates are valid
-------------------------------------------------------------------------------
inBounds :: [[Tile]] -> (Int, Int) -> Bool
inBounds board (x,y)
    | x < 0 || y < 0 = False
    | x >= Data.List.length(board) || y >= Data.List.length(board !! 0) = False
    | otherwise = True

-------------------------------------------------------------------------------
-- rows board - Returns rows limit
-------------------------------------------------------------------------------
rows :: [[a]] -> Int
rows board = (Data.List.length(board)) - 1

-------------------------------------------------------------------------------
-- columns board - Returns columns limit
-------------------------------------------------------------------------------
columns :: [[a]] -> Int
columns board = (Data.List.length(board !! 0)) - 1

-------------------------------------------------------------------------------
-- iterateTile board coord - Iterates through the board row-wise and column-wise
-------------------------------------------------------------------------------
iterateTile :: [[Tile]] -> (Int, Int) -> (Int, Int)
iterateTile board (row,col)
    | col == (columns board) && row == (rows board) = (0,0)
    | col == (columns board) = (row+1,0)
    | otherwise = (row,col+1)

-------------------------------------------------------------------------------
-- iterateTileCol board coord - Iterates through the board column-wise and row-wise
-------------------------------------------------------------------------------
iterateTileCol board (row,col)
    | col == (columns board) && row == (rows board) = (0,0)
    | row == (rows board) = (0,col+1)
    | otherwise = (row+1,col)

-------------------------------------------------------------------------------
-- Move in different directions within the matrix
-------------------------------------------------------------------------------
left (i,j) = (i,j-1)
right (i,j) = (i,j+1)
up (i,j) = (i-1,j)
down (i,j) = (i+1,j)

-------------------------------------------------------------------------------
-- Move N elements in different directions within the matrix
-------------------------------------------------------------------------------
leftN n (i,j) = (i,j+(n*(-1)))
rightN n (i,j) = (i,j+n)
upN n (i,j) = (i+(n*(-1)),j)
downN n (i,j) = (i+(n*1),j)

-------------------------------------------------------------------------------
-- Get tile to the left/right/up/down.
-------------------------------------------------------------------------------
leftTile board coord = readTile board $ left coord
rightTile board coord = readTile board $ right coord
upTile board coord = readTile board $ up coord
downTile board coord = readTile board $ down coord

-------------------------------------------------------------------------------
-- Get n:th tile to the left/right/up/down.
-------------------------------------------------------------------------------
leftNTile n board coord = readTile board $ leftN n coord
rightNTile n board coord = readTile board $ rightN n coord
upNTile n board coord = readTile board $ upN n coord
downNTile n board coord = readTile board $ downN n coord


-------------------------------------------------------------------------------
-- rowFilled l - Filled helper
-------------------------------------------------------------------------------
rowFilled :: [Tile] -> Bool
rowFilled [] = True
rowFilled (x:xs)
    | x == Empty = False
    | otherwise = rowFilled xs

-------------------------------------------------------------------------------
-- filled l - Checks if entire matrix is filled or not (if tiles are Empty)
-------------------------------------------------------------------------------
filled :: [[Tile]] -> Bool
filled [] = True
filled (x:xs)
    | rowFilled x /= True = False
    | otherwise = filled xs

-------------------------------------------------------------------------------
-- readTile board coord - Reads tile in coordinate position
-------------------------------------------------------------------------------
readTile :: [[a]] -> (Int, Int) -> a
readTile board (x,y) = board !! x !! y

-------------------------------------------------------------------------------
-- writeTile board coord val - Updates board value at cordinate coord
-------------------------------------------------------------------------------
writeTile :: [[a]] -> (Int, Int) -> a -> [[a]]
writeTile board (rw,cl) new =
	toList (update rw (getRow board rw cl new) $ fromList board)

-------------------------------------------------------------------------------
-- getRow board coord val - Gets the list containing the updated row from the
-- list of lists with getColumn
-------------------------------------------------------------------------------
getRow :: [[a]] -> Int -> Int -> a -> [a]
getRow (x:xs) 0 cl new = toList (getColumn (fromList x) cl new)
getRow (x:xs) rw cl new = getRow xs (rw-1) cl new

-------------------------------------------------------------------------------
-- getColumn board coord val - Sets the value val in the list at coord
-------------------------------------------------------------------------------
getColumn :: Seq a -> Int -> a -> Seq a
getColumn board cl new = update cl new $ board
