--module Ttl where
import Parser
import Data.Array
import Data.Maybe
import Debug.Trace
import Data.List

adjTwins :: [[Tile]] -> (Int, Int) -> Maybe Tile
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

countElemCol :: (Eq a1, Num a) => [[a1]] -> (Int, Int) -> a1 -> a
countElemCol board (i,j) t
    | i >= (rows board) = 0
    | tile == t = 1 + (countElemCol board (i+1,j) t)
    | otherwise = (countElemCol board (i+1,j) t)
    where tile = readTile board (i,j)

countElemRow :: (Eq a1, Num a) => [[a1]] -> (Int, Int) -> a1 -> a
countElemRow board (i,j) t
    | j >= (columns board) = 0
    | tile == t = 1 + (countElemRow board (i,j+1) t)
    | otherwise = (countElemRow board (i,j+1) t)
    where tile = readTile board (i,j)

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

checkTile :: [[Tile]] -> (Int, Int) -> Maybe Tile
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

<<<<<<< HEAD
findEmptyIndex :: [[Tile]] -> (Int, Int) -> (Int, Int)
=======
>>>>>>> 9426896ac2885d055176b3176a5580abbc056cc1
findEmptyIndex board coord
    | (readTile board coord) == Empty = coord
    | nextTile == (0,0) = error (show board)
    | otherwise = findEmptyIndex board nextTile
    where nextTile = iterateTile board coord

<<<<<<< HEAD
fillFirstEmpty :: [[Tile]] -> Tile -> [[Tile]]
fillFirstEmpty board t = writeTile board (findEmptyIndex board (0,0)) t

dfs :: [[[Tile]]] -> [[Tile]]
=======
fillFirstEmpty board t = writeTile board (findEmptyIndex board (0,0)) t

--guess board
--    | filled x && (validate x) = x
--    | filled o && (validate o)  = o
--    | otherwise = error "No Solution"
--    where
--        x = solve
--        o = solve (fillFirstEmpty board O)

>>>>>>> 9426896ac2885d055176b3176a5580abbc056cc1
dfs [] = error "No solution homie!"
dfs (board:boards)
    | full && validate newBoard = newBoard
    | full && not (validate newBoard) = dfs boards
    | not changed =
        let x = (fillFirstEmpty board X)
            o = (fillFirstEmpty board O)
        in dfs (x:o:boards)
    | otherwise = dfs (newBoard:boards)
    where
        (changed, newBoard) = solve' board (0,0) False
        full = filled newBoard
<<<<<<< HEAD
=======

--solve board
--    | full && (validate newBoard) = newBoard
--    | full && not (validate newBoard) = error "NoSolu"
--    | not changed = guess board
--    | otherwise = solve newBoard
--    where
--        (changed, newBoard) = solve' board (0,0) False
--        full = filled newBoard
 --       guessBoard = guess newBoard
>>>>>>> 9426896ac2885d055176b3176a5580abbc056cc1

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

<<<<<<< HEAD
checkRow :: Eq a => a -> [a] -> Bool
=======
>>>>>>> 9426896ac2885d055176b3176a5580abbc056cc1
checkRow row [] = True
checkRow row (x:xs)
    | row == x = False
    | otherwise = checkRow row xs

<<<<<<< HEAD
checkRows :: Eq t => [t] -> Bool
=======
>>>>>>> 9426896ac2885d055176b3176a5580abbc056cc1
checkRows [] = True
checkRows (x:xs)
    | not check = False
    | otherwise = checkRows xs
    where
        check = checkRow x xs

<<<<<<< HEAD
validate :: Eq a => [[a]] -> Bool
=======
>>>>>>> 9426896ac2885d055176b3176a5580abbc056cc1
validate board
    | cols && rows = True
    | otherwise = False
    where
        cols = checkRows $ transpose board
        rows = checkRows board

<<<<<<< HEAD
printBoard :: Show a => [[a]] -> IO ()
=======

>>>>>>> 9426896ac2885d055176b3176a5580abbc056cc1
printBoard [] = putStr "\n"
printBoard (x:xs) = do
    printRow x
    printBoard xs

printRow :: Show a => [a] -> IO ()
printRow [] = putStr "\n"
printRow (x:xs) = do
    putStr (show x)
    printRow xs

lineToRead :: IO[Int]
lineToRead = (map read . words) `fmap` getLine

main :: IO ()
main = do
    board <- getBoard
    printBoard $ dfs [board]
