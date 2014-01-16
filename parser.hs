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


readWords :: (Read a) => String -> [a]
readWords = map read . words
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

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

readLine = (map toTile) `fmap` getLine

writeTile board rw cl new = toList (update rw (getRow board rw cl new) $ fromList board)
getRow (x:xs) 0 cl new = toList (getColumn (fromList x) cl new)
getRow (x:xs) rw cl new = getRow xs (rw-1) cl new
getColumn board cl new = update cl new $ board

readTile board x y = board !! x !! y
