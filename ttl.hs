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

elemFilled m (i, j) = False -- Does row/col contain n/2 of any element?

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
    | otherwise = Nothing
    where
        btwn = inBetween board coord
        adj = adjTwins board coord


guess = False -- Randomly picks an element and dfs that shizz

solve board = solve' board (0,0)
solve' board coord
    | tileResult /= Nothing = 
        let 
            newBoard = writeTile board coord (fromJust tileResult)
        in
            solve' newBoard nextTile
    | nextTile == (0,0) = board
    | otherwise = solve' board nextTile
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
