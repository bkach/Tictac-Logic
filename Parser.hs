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

opposite X = O
opposite O = X
opposite Empty = Empty

inBounds board (x,y)
    | x < 0 || y < 0 = False
    | x >= Data.List.length(board) || y >= Data.List.length(board !! 0) = False
    | otherwise = True

rows board = (Data.List.length(board)) - 1
columns board = (Data.List.length(board !! 0)) - 1

iterateTile board (row,col)
    | col == (columns board) &&
        row == (rows board) = (0,0)
    | col == (columns board) = (row+1,0)
    | otherwise = (row,col+1)

readLine = (map toTile) `fmap` getLine

readWords :: (Read a) => String -> [a]
readWords = map read . words

left (i,j) = (i,j-1)
right (i,j) = (i,j+1)
up (i,j) = (i-1,j)
down (i,j) = (i+1,j)

leftN n (i,j) = (i,j+(n*(-1)))
rightN n (i,j) = (i,j+n)
upN n (i,j) = (i+(n*(-1)),j)
downN n (i,j) = (i+(n*1),j)

leftNTile n board (i,j) = readTile board (i,j+(n*(-1)))
rightNTile n board (i,j) = readTile board (i,j+n)
upNTile n board (i,j) = readTile board (i+(n*(-1)),j)
downNTile n board (i,j) = readTile board (i+(n*1),j)

leftTile board (i,j) = readTile board (i,j-1)
rightTile board (i,j) = readTile board (i,j+1)
upTile board (i,j) = readTile board (i-1,j)
downTile board (i,j) = readTile board (i+1,j)

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

writeTile board (rw,cl) new = toList (update rw (getRow board rw cl new) $ fromList board)
getRow (x:xs) 0 cl new = toList (getColumn (fromList x) cl new)
getRow (x:xs) rw cl new = getRow xs (rw-1) cl new
getColumn board cl new = update cl new $ board
