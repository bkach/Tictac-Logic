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
-- avoidTriplets - Checks to see if we can fill a tile at coord by avoiding
-- triplets in row or column
-- Example .X..OXXO -> OX..OXXO
-------------------------------------------------------------------------------
avoidTriplets board coord
    | inBounds board (rightN 4 coord) &&
        numRowTiles board coord == 3 &&
        right /= Empty &&
        rightNTile 2 board coord == Empty &&
        rightNTile 3 board coord == Empty &&
        rightNTile 4 board coord == opposite right =
            Just $ opposite right
    | inBounds board (leftN 4 coord) &&
        numRowTiles board coord == 3 &&
        left /= Empty &&
        leftNTile 2 board coord == Empty &&
        leftNTile 3 board coord == Empty &&
        leftNTile 4 board coord == opposite left =
            Just $ opposite left
    | inBounds board (upN 4 coord) &&
        numColTiles board coord == 3 &&
        up /= Empty &&
        upNTile 2 board coord == Empty &&
        upNTile 3 board coord == Empty &&
        upNTile 4 board coord == opposite up =
            Just $ opposite up
    | inBounds board (downN 4 coord) &&
        numColTiles board coord == 3 &&
        down /= Empty &&
        downNTile 2 board coord == Empty &&
        downNTile 3 board coord == Empty &&
        downNTile 4 board coord == opposite down =
            Just $ opposite down
    | otherwise = Nothing
    where
        right = rightTile board coord
        left = leftTile board coord
        up = upTile board coord
        down = downTile board coord

-------------------------------------------------------------------------------
-- avoidDupRow board coord - When only two empty tiles exists in a row we
-- tests to see what values would create a duplicate row, we return the
-- opposite of that
-------------------------------------------------------------------------------
avoidDupRow board coord
    | numRowTiles board coord == 2 &&
     countElemRow board coord X == countElemRow board coord O  =
        let xRowFilled = writeTile board coord X
            oRowFilled = setFirstEmptyRow xRowFilled (i, 0) O
            rowUnique = checkRows oRowFilled && checkRows (transpose oRowFilled)
        in case rowUnique of
            True -> Just X
            _ -> Just O
    | otherwise = Nothing
    where
        (i,j) = coord

-------------------------------------------------------------------------------
-- avoidDupCol board coord - When only two empty tiles exists in a column we
-- tests to see what values would create a duplicate column, we return the
-- opposite of that
-------------------------------------------------------------------------------
avoidDupCol board coord
    | numColTiles board coord == 2 &&
     countElemCol board coord X == countElemCol board coord O  =
        let xColFilled = writeTile board coord X
            oColFilled = setFirstEmptyCol xColFilled (0, j) O
            colUnique = checkRows (transpose oColFilled) && checkRows oColFilled
        in case colUnique of
            True -> Just X
            _ -> Just O
    | otherwise = Nothing
    where
        (i,j) = coord
-------------------------------------------------------------------------------
avoidDupRow2 board coord
    | noX == (halfRow - 1) && noO < noX  && (not $ coord `elem` twinEmptys) =
            Just O
    | noO == (halfRow - 1) && noX < noO  && (not $ coord `elem` twinEmptys) =
            Just X
    | otherwise = Nothing
    where
        (i,j) = coord
        halfRow = (columns board +1) `div` 2
        noX = countElemRow board (i, 0) X
        noO= countElemRow board (i, 0) O
        twinEmptys = rowTwinEmpty board (i,0) []

avoidDupCol2 board coord
    | noX == (halfCol - 1) && noO < noX  && (not $ coord `elem` twinEmptys) =
            Just O
    | noO == (halfCol - 1) && noX < noO  && (not $ coord `elem` twinEmptys) =
            Just X
    | otherwise = Nothing
    where
        (i,j) = coord
        halfCol = (rows board +1) `div` 2
        noX = countElemCol board (0,j) X
        noO = countElemCol board (0,j) O
        twinEmptys = colTwinEmpty board (0,j) []

rowTwinEmpty board (i,j) acc
    | j > (columns board) = []
    | tile == Empty = rowTwinEmpty board (right coord) (coord:acc)
    | length(acc) == 2 = acc
    | otherwise = rowTwinEmpty board (right coord) []
    where
        coord = (i,j)
        tile = readTile board coord

colTwinEmpty board (i,j) acc
    | i > (rows board) = []
    | tile == Empty = colTwinEmpty board (down coord) (coord:acc)
    | length(acc) == 2 = acc
    | otherwise = colTwinEmpty board (down coord) []
    where
        coord = (i,j)
        tile = readTile board coord
-------------------------------------------------------------------------------
-- numRowTiles board coord - Counts number of empty tiles in a row
-------------------------------------------------------------------------------
numRowTiles' board (i,j)
    | j > (columns board) = 0
    | tile == Empty = 1 + numRowTiles' board (right coord)
    | otherwise = numRowTiles' board (right coord)
    where
        coord = (i,j)
        tile = readTile board coord
numRowTiles board (i,j) = numRowTiles' board (i, 0)


numColTiles' board (i,j)
    | i > (rows board) = 0
    | tile == Empty = 1 + numColTiles' board (down coord)
    | otherwise = numColTiles' board (down coord)
    where
        coord = (i,j)
        tile = readTile board coord

-------------------------------------------------------------------------------
-- numColTiles board coord - Counts number of empty tiles in a column
-------------------------------------------------------------------------------
numColTiles board (i,j) =
    numColTiles' board (0, j)

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
-- findEmptyIndex board coord - Finds the index of the first empty tile in board
-- (row wise)
-------------------------------------------------------------------------------
findEmptyIndex :: [[Tile]] -> (Int, Int) -> (Int, Int)
findEmptyIndex board coord
    | (readTile board coord) == Empty = coord
    | nextTile == (0,0) = error (show board)
    | otherwise = findEmptyIndex board nextTile
    where nextTile = iterateTile board coord

-------------------------------------------------------------------------------
-- fillFirstEmptyCol board t - Finds the index of the first empty tile in board
-- (column wise)
-------------------------------------------------------------------------------
findEmptyIndexCol :: [[Tile]] -> (Int, Int) -> (Int, Int)
findEmptyIndexCol board coord
    | (readTile board coord) == Empty = coord
    | nextTile == (0,0) = error (show board)
    | otherwise = findEmptyIndexCol board nextTile
    where nextTile = iterateTileCol board coord

setFirstEmptyCol :: [[Tile]] -> (Int, Int) -> Tile -> [[Tile]]
setFirstEmptyCol board (i,j) t = writeTile board (findEmptyIndexCol board (0,j)) t

setFirstEmptyRow :: [[Tile]] -> (Int, Int) -> Tile -> [[Tile]]
setFirstEmptyRow board (i,j) t = writeTile board (findEmptyIndex board (i,0)) t

-------------------------------------------------------------------------------
-- fillFirstEmpty board t - Sets the first empty tile in board with t
-------------------------------------------------------------------------------
setFirstEmpty :: [[Tile]] -> Tile -> [[Tile]]
setFirstEmpty board t = writeTile board (findEmptyIndex board (0,0)) t

-------------------------------------------------------------------------------
-- checkTile board coord - Tries to returns what to fill the tile at coord with
-- by checking the above rules
-------------------------------------------------------------------------------
checkTile :: [[Tile]] -> (Int, Int) -> Maybe Tile
checkTile board coord
    | readTile board coord /= Empty = Nothing
    | btwn /= Nothing = btwn
    | adj /= Nothing = adj
    -- | triplets /= Nothing = triplets
    | half /= Nothing = half
    -- | dupRow /= Nothing = dupRow
    -- | dupCol /= Nothing = dupCol
    -- | dupRow2 /= Nothing = dupRow2
    -- | dupCol2 /= Nothing = dupCol2
    | otherwise = Nothing
    where
        btwn = inBetween board coord
        adj = adjTwins board coord
        half = elemFilled board coord
        triplets = avoidTriplets board coord
        dupRow2 = avoidDupRow2 board coord
        dupCol2 = avoidDupCol2 board coord
        dupRow = avoidDupRow board coord
        dupCol = avoidDupCol board coord

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
    | not (validate newBoard) = solve boards
    | full && validate newBoard = newBoard
    | full && not (validate newBoard) = solve boards
    | not changed =
        let x = (setFirstEmpty board X)
            o = (setFirstEmpty board O)
        in solve (x:o:boards)
       --in error (show newBoard)
    | otherwise = solve (newBoard:boards)
    where
        (changed, newBoard) = solve' board (0,0) False
        full = filled newBoard

-------------------------------------------------------------------------------
-- checkRow row rows - checkRows helper, compares row against all elements in rows
-------------------------------------------------------------------------------
checkTriplets' :: [Tile] -> Bool
checkTriplets' [] = False
checkTriplets' (first:xs)
    | first /= Empty && length(xs) >= 2 && (first == second) && (second == third) = True
    | otherwise = checkTriplets' xs
    where
        second = head(xs)
        third = xs !! 1

checkTriplets :: [[Tile]] -> Bool
checkTriplets [] = False
checkTriplets (x:xs)
    | check = True
    | otherwise = checkTriplets xs
    where check = checkTriplets' x

-------------------------------------------------------------------------------
-- containsEmpty row - Checks if row contains Empty Tile
-------------------------------------------------------------------------------
containsEmpty :: [Tile] -> Bool
containsEmpty [] = False
containsEmpty (x:xs)
    | x == Empty = True
    | otherwise = containsEmpty xs

-------------------------------------------------------------------------------
-- checkRow row rows - checkRows helper, compares row against all elements in rows
-------------------------------------------------------------------------------
checkRow :: [Tile] => [[Tile]] -> Bool
checkRow row [] = True
checkRow row (x:xs)
    | containsEmpty x = checkRow row xs
    | row == x = False
    | otherwise = checkRow row xs

-------------------------------------------------------------------------------
-- checkRows rows - validate helper
-------------------------------------------------------------------------------
checkRows :: [[Tile]] -> Bool
checkRows [] = True
checkRows (x:xs)
    | not check = False
    | otherwise = checkRows xs
    where
        check = checkRow x xs

equallXO' [] xs os = xs == os
equallXO' (t:ts) xs os
    | t == X = equallXO' ts (1+xs) os
    | t == O = equallXO' ts xs (1+os)
    | otherwise = True

equallXO [] = True
equallXO (x:xs)
    | not check = False
    | otherwise = equallXO xs
        where check = equallXO' x 0 0

-------------------------------------------------------------------------------
-- validate board - Checks if pairs of rows and columns are unique
-------------------------------------------------------------------------------
validate :: [[Tile]] -> Bool
validate board
    | cols && rows && tripletsCol && tripletsRow && equalRow && equalCol = True
    | otherwise = False
    where
        cols = checkRows $ transpose board
        rows = checkRows board
        tripletsCol = not $ checkTriplets $ transpose board
        tripletsRow = not $ checkTriplets board
        equalRow = equallXO board
        equalCol = equallXO $ transpose board


