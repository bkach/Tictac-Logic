import Tile
import Matrix
import Solver

import Control.Monad

readLine = (map toTile) `fmap` getLine

readWords :: (Read a) => String -> [a]
readWords = map read . words

lineToReadInt :: IO[Int]
lineToReadInt = readWords `fmap` getLine

lineToReadStr :: IO[String]
lineToReadStr = lines `fmap` getLine

lineToRead :: IO[Int]
lineToRead = readWords `fmap` getLine

printRow :: Show a => [a] -> IO ()
printRow [] = putStr "\n"
printRow (x:xs) = do
    putStr (show x)
    printRow xs

printBoard :: Show a => [[a]] -> IO ()
printBoard [] = putStr "\n"
printBoard (x:xs) = do
    printRow x
    printBoard xs

getBoard = do
    ln <- lineToReadInt
    let rws = ln !! 0
        cls = ln !! 1
    board <- replicateM rws readLine
    return board

main :: IO ()
main = do
    board <- getBoard
    printBoard $ solve [board]

