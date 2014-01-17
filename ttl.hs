import Parser
import Data.Array
import Data.Maybe
import Debug.Trace

type Position = (Int, Int)


adjTwins board coord
    | Parser.inBounds board (leftN 2 coord) &&
        (leftTile board coord) /= Empty &&
            ((leftTile board coord) == (leftNTile 2 board coord)) =
                Just (opposite (leftTile board coord))
    | Parser.inBounds board (rightN 2 coord) &&
        (rightTile board coord) /= Empty &&
            ((rightTile board coord) == (rightNTile 2 board coord)) =
                Just (opposite (rightTile board coord))
    | Parser.inBounds board (upN 2 coord) &&
        (upTile board coord) /= Empty &&
            ((upTile board coord) == (upNTile 2 board coord)) =
                Just (opposite (upTile board coord))
    | Parser.inBounds board (downN 2 coord) &&
        (downTile board coord) /= Empty &&
            ((downTile board coord) == (downNTile 2 board coord)) =
                Just (opposite (downTile board coord))
    | otherwise = Nothing

countElemCol board (i,j) t
    | i >= (rows board) = 0
    | tile == t = 1 + (countElemCol board (i+1,j) t)
    | otherwise = (countElemCol board (i+1,j) t)
    where tile = readTile board (i,j)

countElemRow board (i,j) t
    | j >= (columns board) = 0
    | tile == t = 1 + (countElemRow board (i,j+1) t)
    | otherwise = (countElemRow board (i,j+1) t)
    where tile = readTile board (i,j)

elemFilled board (i,j)
    | countElemCol board (0, j) X == halfRow = Just O
    | countElemCol board (0, j) O == halfRow = Just X
    | countElemRow board (i, 0) X == halfCol = Just O
    | countElemRow board (i, 0) O == halfCol = Just X
    | otherwise = Nothing
    where
        halfRow = rows board `div` 2
        halfCol = columns board `div` 2

inBetween board coord
    | Parser.inBounds board (left coord) &&
        (leftTile board coord) /= Empty &&
            Parser.inBounds board (right coord) &&
                ((leftTile board coord) == (rightTile board coord)) =
                    Just (opposite (leftTile board coord))
    | Parser.inBounds board (up coord) &&
        (upTile board coord) /= Empty &&
            Parser.inBounds board (down coord) &&
                ((upTile board coord) == (downTile board coord)) =
                    Just (opposite (upTile board coord))
    | otherwise = Nothing


checkTile board coord
    | readTile board coord /= Empty =
        Nothing
    | btwn /= Nothing =
        btwn
    | adj /= Nothing =
        adj
    | half /= Nothing =
        half
    | otherwise = Nothing
    where
        btwn = inBetween board coord
        adj = adjTwins board coord
        half = elemFilled board coord

guess = False -- Randomly picks an element and dfs that shizz

solve board =
    -- | changed = solve newBoard
    -- | filled = newBoard --Guessing
    -- | otherwise =
        newBoard
    where
        (changed, newBoard) = solve' board (0,0) False

solve' board coord changed
    | tileResult /= Nothing =
        let
            newBoard = writeTile board coord (fromJust tileResult)
        in
            solve' newBoard nextTile True
    | nextTile == (0,0) = (changed, board)
    | otherwise = solve' board nextTile changed
    where
        tileResult = checkTile board coord
        nextTile = iterateTile board coord

printBoard [] = putStr "\n"
printBoard (x:xs) = do
    printRow x
    printBoard xs
printRow [] = putStr "\n"
printRow (x:xs) = do
    putStr (show x)
    printRow xs

--validate :: Matrix -> Bool
validate m = False

lineToRead :: IO[Int]
lineToRead = (map read . words) `fmap` getLine

main :: IO ()
main = do
    board <- getBoard
    printBoard (solve board)
