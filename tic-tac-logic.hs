import Tile
import Matrix
import Solver

import Control.Monad

-------------------------------------------------------------------------------
-- IO helpers
-------------------------------------------------------------------------------
readLine :: IO[Tile]
readLine = (map toTile) `fmap` getLine

readWords :: (Read a) => String -> [a]
readWords = map read . words

lineToReadInt :: IO[Int]
lineToReadInt = readWords `fmap` getLine

lineToReadStr :: IO[String]
lineToReadStr = lines `fmap` getLine

lineToRead :: IO[Int]
lineToRead = readWords `fmap` getLine

-------------------------------------------------------------------------------
-- printTow l - printBoard helper function, prints eachrow with a linebreak
-------------------------------------------------------------------------------
printRow :: Show a => [a] -> IO ()
printRow [] = putStr "\n"
printRow (x:xs) = do
    putStr (show x)
    printRow xs

-------------------------------------------------------------------------------
-- printBoard board - Prints entire board in the original input format
-------------------------------------------------------------------------------
printBoard :: Show a => [[a]] -> IO ()
printBoard [] = putStr "\n"
printBoard (x:xs) = do
    printRow x
    printBoard xs

-------------------------------------------------------------------------------
-- getBoard - Reads enitre board from input into [[Tile]]
-------------------------------------------------------------------------------
getBoard :: IO [[Tile]]
getBoard = do
    ln <- lineToReadInt
    let rws = ln !! 0
        cls = ln !! 1
    board <- replicateM rws readLine
    return board

-------------------------------------------------------------------------------
-- filled l - Checks if entire matrix is filled or not (if tiles are Empty)
-------------------------------------------------------------------------------
main :: IO ()
main = do
    board <- getBoard
    printBoard $ solve [board]

