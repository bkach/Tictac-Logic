import Parser
import Data.Array
import Data.Maybe

type Position = (Int, Int)


checkLeft board (i,j)
    | Parser.inBounds board (i, j-2) = 
        (left board (i, j)) == (leftN 2 board (i, j))
    | otherwise = False

checkRight board (i,j)
    | Parser.inBounds board (i, j+2) = 
        (right board (i, j)) == (rightN 2 board (i, j))
    | otherwise = False

checkUp board (i,j)
    | Parser.inBounds board (i-2, j) = 
        (up board (i, j)) == (upN 2 board (i, j))
    | otherwise = False

checkDown board (i,j)
    | Parser.inBounds board (i+2, j) = 
        (down board (i, j)) == (downN 2 board (i, j))

neighbourToPair board coord
    | checkLeft board coord = Just (left board coord)
    | checkRight board coord = Just (right board coord)
    | checkUp board coord = Just (up board coord)
    | checkDown board coord = Just (down board coord)
    | otherwise = Nothing
    
    -- Checks to see if neighbours have two elements appearing after eachother Maybe Int
   

elemFilled m (i, j) = False -- Does row/col contain n/2 of any element?

--inBetweenRow m (i, j) =
inBetween m (i, j) = False -- Is element in between 2 elements of the same value?

guess = False -- Randomly picks an element and dfs that shizz

solve m =
    False -- Solves binero

solver = False

printBoard [] = putStr "\n"
printBoard (x:xs) = do
    printRow x
    printBoard xs
printRow [] = putStr "\n"
printRow (x:xs) = do
    putStr (show x)
    printRow xs

-- Validate solution
--validate :: Matrix -> Bool
validate m = False

lineToRead :: IO[Int]
lineToRead = (map read . words) `fmap` getLine

main :: IO ()
main = do
    board <- getBoard
    printBoard board
