module Solver where

import Matrix
import Tile

import Data.Maybe
import Data.List

-------------------------------------------------------------------------------
-- adjTwins board coord - Rule that checks if Tile at cordinate coord has a pair
-- of equal neighbours and returns the opposite of that.
-- Example .OO => X
-------------------------------------------------------------------------------
adjTwins :: [[Tile]] -> (Int, Int) -> Maybe Tile
adjTwins board coord
    | inBounds board (leftN 2 coord) &&
        (leftTile board coord) /= Empty &&
            ((leftTile board coord) == (leftNTile 2 board coord)) =
                Just (opposite (leftTile board coord))
    | inBounds board (rightN 2 coord) &&
        (rightTile board coord) /= Empty &&
            ((rightTile board coord) == (rightNTile 2 board coord)) =
                Just (opposite (rightTile board coord))
    | inBounds board (upN 2 coord) &&
        (upTile board coord) /= Empty &&
            ((upTile board coord) == (upNTile 2 board coord)) =
                Just (opposite (upTile board coord))
    | inBounds board (downN 2 coord) &&
        (downTile board coord) /= Empty &&
            ((downTile board coord) == (downNTile 2 board coord)) =
                Just (opposite (downTile board coord))
    | otherwise = Nothing

-------------------------------------------------------------------------------
-- coundElemCol board coord tile - Counts the number of tiles that are equal
-- to t in a column, elemFilled helper
-------------------------------------------------------------------------------
countElemCol :: (Eq a1, Num a) => [[a1]] -> (Int, Int) -> a1 -> a
countElemCol board (i,j) t
    | i > (rows board) = 0
    | tile == t = 1 + countElemCol board (down coord) t
    | otherwise = countElemCol board (down coord) t
    where
        coord = (i,j)
        tile = readTile board coord

-------------------------------------------------------------------------------
-- coundElemRow board coord tile - Counts the number of tiles that are equal
-- to t in a row, elemFilled helper
-------------------------------------------------------------------------------
countElemRow :: (Eq a1, Num a) => [[a1]] -> (Int, Int) -> a1 -> a
countElemRow board (i,j) t
    | j > (columns board) = 0
    | tile == t = 1 + countElemRow board (right coord) t
    | otherwise = countElemRow board (right coord) t
    where
        coord = (i,j)
        tile = readTile board coord

-------------------------------------------------------------------------------
-- elemFilled board coord - Rule to check and see if it is possible to fill a
-- tile by mutual exclusion. That is, a row or column already have enough O:s or X:s
-------------------------------------------------------------------------------
elemFilled :: [[Tile]] -> (Int, Int) -> Maybe Tile
elemFilled board (i,j)
    | countElemCol board (0, j) X == halfCol = Just O
    | countElemCol board (0, j) O == halfCol = Just X
    | countElemRow board (i, 0) X == halfRow = Just O
    | countElemRow board (i, 0) O == halfRow = Just X
    | otherwise = Nothing
    where
        halfCol = (rows board +1)`div` 2
        halfRow = (columns board +1) `div` 2

-------------------------------------------------------------------------------
-- inBetween board coord - Rule to check if an empty tile is between to similar
-- tiles, and returns the opposite of that. Example X.X => O
-------------------------------------------------------------------------------
inBetween :: [[Tile]] -> (Int, Int) -> Maybe Tile
inBetween board coord
    | inBounds board (left coord) &&
        (leftTile board coord) /= Empty &&
            inBounds board (right coord) &&
                ((leftTile board coord) == (rightTile board coord)) =
                    Just (opposite (leftTile board coord))
    | inBounds board (up coord) &&
        (upTile board coord) /= Empty &&
            inBounds board (down coord) &&
                ((upTile board coord) == (downTile board coord)) =
                    Just (opposite (upTile board coord))
    | otherwise = Nothing

-------------------------------------------------------------------------------
-- checkTile board coord - Tries to returns what to fill the tile at coord with
-- by checking the above rules
-------------------------------------------------------------------------------
checkTile :: [[Tile]] -> (Int, Int) -> Maybe Tile
checkTile board coord
    | readTile board coord /= Empty = Nothing
    | btwn /= Nothing = btwn
    | adj /= Nothing = adj
    | half /= Nothing = half
    | otherwise = Nothing
    where
        btwn = inBetween board coord
        adj = adjTwins board coord
        half = elemFilled board coord

-------------------------------------------------------------------------------
-- findEmptyIndex board coord - Finds the index of the first empty tile in board
-------------------------------------------------------------------------------
findEmptyIndex :: [[Tile]] -> (Int, Int) -> (Int, Int)
findEmptyIndex board coord
    | (readTile board coord) == Empty = coord
    | nextTile == (0,0) = error (show board)
    | otherwise = findEmptyIndex board nextTile
    where nextTile = iterateTile board coord

-------------------------------------------------------------------------------
-- fillFirstEmpty board t - Sets the first empty tile in board with t
-------------------------------------------------------------------------------
setFirstEmpty :: [[Tile]] -> Tile -> [[Tile]]
setFirstEmpty board t = writeTile board (findEmptyIndex board (0,0)) t

-------------------------------------------------------------------------------
-- solve' board coord changed - Goes through entire board once and tries to
-- fill each tile by applying rules. Returns the new board and if it succeded
-- at setting a tile in the board.
-------------------------------------------------------------------------------
solve' :: [[Tile]] -> (Int, Int) -> Bool -> (Bool, [[Tile]])
solve' board coord changed
    | tileResult /= Nothing =
        let newBoard = writeTile board coord (fromJust tileResult)
        in solve' newBoard nextTile True
    | nextTile == (0,0) = (changed, board)
    | otherwise = solve' board nextTile changed
    where
        tileResult = checkTile board coord
        nextTile = iterateTile board coord

-------------------------------------------------------------------------------
-- solve [board] - Tries to solve each board by using solve' (rule based),
-- otherwise puts 2 new boards into the seach queue with an X and an O at the
-- first empty tile. Searching is done in a DFS way.
-- Returns solved board or throws an error.
-------------------------------------------------------------------------------
solve :: [[[Tile]]] -> [[Tile]]
solve [] = error "No solution found!"
solve (board:boards)
    | full && validate newBoard = newBoard
    | full && not (validate newBoard) = solve boards
    | not changed =
        let x = (setFirstEmpty board X)
            o = (setFirstEmpty board O)
        in solve (x:o:boards)
    | otherwise = solve (newBoard:boards)
    where
        (changed, newBoard) = solve' board (0,0) False
        full = filled newBoard

-------------------------------------------------------------------------------
-- checkRow row rows - checkRows helper, compares row against all elements in rows
-------------------------------------------------------------------------------
checkRow :: Eq a => a -> [a] -> Bool
checkRow row [] = True
checkRow row (x:xs)
    | row == x = False
    | otherwise = checkRow row xs

-------------------------------------------------------------------------------
-- checkRows rows - validate helper
-------------------------------------------------------------------------------
checkRows :: Eq t => [t] -> Bool
checkRows [] = True
checkRows (x:xs)
    | not check = False
    | otherwise = checkRows xs
    where check = checkRow x xs
-------------------------------------------------------------------------------
-- validate board - Checks if pairs of rows and columns are unique
-------------------------------------------------------------------------------
validate :: Eq a => [[a]] -> Bool
validate board
    | cols && rows = True
    | otherwise = False
    where
        cols = checkRows $ transpose board
        rows = checkRows board