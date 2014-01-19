module Solver where

import Matrix
import Tile

import Data.Maybe
import Data.List

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

countElemCol :: (Eq a1, Num a) => [[a1]] -> (Int, Int) -> a1 -> a
countElemCol board (i,j) t
    | i >= (rows board) = 0
    | tile == t = 1 + countElemCol board (down coord) t
    | otherwise = countElemCol board (down coord) t
    where
        coord = (i,j)
        tile = readTile board coord

countElemRow :: (Eq a1, Num a) => [[a1]] -> (Int, Int) -> a1 -> a
countElemRow board (i,j) t
    | j >= (columns board) = 0
    | tile == t = 1 + countElemRow board (right coord) t
    | otherwise = countElemRow board (right coord) t
    where
        coord = (i,j)
        tile = readTile board coord

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

findEmptyIndex :: [[Tile]] -> (Int, Int) -> (Int, Int)
findEmptyIndex board coord
    | (readTile board coord) == Empty = coord
    | nextTile == (0,0) = error (show board)
    | otherwise = findEmptyIndex board nextTile
    where nextTile = iterateTile board coord

fillFirstEmpty :: [[Tile]] -> Tile -> [[Tile]]
fillFirstEmpty board t = writeTile board (findEmptyIndex board (0,0)) t

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

solve :: [[[Tile]]] -> [[Tile]]
solve [] = error "No solution found!"
solve (board:boards)
    | full && validate newBoard = newBoard
    | full && not (validate newBoard) = solve boards
    | not changed =
        let x = (fillFirstEmpty board X)
            o = (fillFirstEmpty board O)
        in solve (x:o:boards)
    | otherwise = solve (newBoard:boards)
    where
        (changed, newBoard) = solve' board (0,0) False
        full = filled newBoard

checkRow :: Eq a => a -> [a] -> Bool
checkRow row [] = True
checkRow row (x:xs)
    | row == x = False
    | otherwise = checkRow row xs

checkRows :: Eq t => [t] -> Bool
checkRows [] = True
checkRows (x:xs)
    | not check = False
    | otherwise = checkRows xs
    where check = checkRow x xs

validate :: Eq a => [[a]] -> Bool
validate board
    | cols && rows = True
    | otherwise = False
    where
        cols = checkRows $ transpose board
        rows = checkRows board

