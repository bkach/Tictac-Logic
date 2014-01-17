module Parser where

import Test.HUnit
import Data.Foldable
import Control.Monad
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.All
import Debug.Trace
import Data.Array
import Data.List
import Data.Sequence


data Tile = Empty | X | O

instance Show Tile where
    show (Empty) = "."
    show (X) = "X"
    show (O) = "O"

instance Eq Tile where
    X == X = True
    Empty == Empty = True
    O == O = True
    _ == _  = False

toTile '.' = Empty
toTile 'X' = X
toTile 'O' = O
toTile p = error [p]

inBounds board (x,y)
    | x < 0 || y < 0 = False
    | x >= Data.List.length(board) || y >= Data.List.length(board !! 0) = False
    | otherwise = True

readLine = (map toTile) `fmap` getLine

readWords :: (Read a) => String -> [a]
readWords = map read . words

leftN n board (i,j) = readTile board (i,j+(n*(0)))
left board (i,j) = readTile board (i,j-1)
rightN n board (i,j) = readTile board (i,j+n)
right board (i,j) = readTile board (i,j+1)
upN n board (i,j) = readTile board (i+(n*(-1)),j)
up board (i,j) = readTile board (i-1,j)
downN n board (i,j) = readTile board (i+(n*1),j)
down board (i,j) = readTile board (i+1,j)

lineToReadInt :: IO[Int]
lineToReadInt = (readWords) `fmap` getLine
lineToReadStr :: IO[String]
lineToReadStr = (lines) `fmap` getLine

getBoard = do
    ln <- lineToReadInt
    let rws = ln !! 0
        cls = ln !! 1
    board <- Control.Monad.replicateM rws readLine
    return board


readTile board (x,y) = board !! x !! y

writeTile board rw cl new = toList (update rw (getRow board rw cl new) $ fromList board)
getRow (x:xs) 0 cl new = toList (getColumn (fromList x) cl new)
getRow (x:xs) rw cl new = getRow xs (rw-1) cl new
getColumn board cl new = update cl new $ board
