{-# LANGUAGE TemplateHaskell #-}
import Test.HUnit
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All
import Debug.Trace
import Data.List

data Tile = Empty | X | O
instance Show Tile where
    show (Empty) = "."
    show (X) = "X"
    show (O) = "O"

lineToReadInt :: IO[Int]
lineToReadInt = (map read . words) `fmap` getLine

lineToReadStr:: IO[String]
lineToReadStr = (lines) `fmap` getLine

main :: IO ()
main = do
    ln <- lineToReadInt
    let rws = ln !! 0
        cls = ln !! 1
    test <- readLine(cls)
    [test] <- lineToReadStr
    print $ map toTile test

toTile '.' = Empty
toTile 'X' = X
toTile 'O' = O
