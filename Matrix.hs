module Matrix where

import Tile
import Data.Sequence
import Data.List
import Data.Foldable

inBounds board (x,y)
    | x < 0 || y < 0 = False
    | x >= Data.List.length(board) || y >= Data.List.length(board !! 0) = False
    | otherwise = True

rows board = (Data.List.length(board)) - 1
columns board = (Data.List.length(board !! 0)) - 1

iterateTile board (row,col)
    | col == (columns board) && row == (rows board) = (0,0)
    | col == (columns board) = (row+1,0)
    | otherwise = (row,col+1)

left (i,j) = (i,j-1)
right (i,j) = (i,j+1)
up (i,j) = (i-1,j)
down (i,j) = (i+1,j)

leftN n (i,j) = (i,j+(n*(-1)))
rightN n (i,j) = (i,j+n)
upN n (i,j) = (i+(n*(-1)),j)
downN n (i,j) = (i+(n*1),j)

leftTile board coord = readTile board $ left coord
rightTile board coord = readTile board $ right coord
upTile board coord = readTile board $ up coord
downTile board coord = readTile board $ down coord

leftNTile n board coord = readTile board $ leftN n coord
rightNTile n board coord = readTile board $ rightN n coord
upNTile n board coord = readTile board $ upN n coord
downNTile n board coord = readTile board $ downN n coord

filled [] = True
filled (x:xs)
    | rowFilled x /= True = False
    | otherwise = filled xs
rowFilled [] = True
rowFilled (x:xs)
    | x == Empty = False
    | otherwise = rowFilled xs


readTile board (x,y) = board !! x !! y

writeTile board (rw,cl) new =
	toList (update rw (getRow board rw cl new) $ fromList board)

getRow (x:xs) 0 cl new = toList (getColumn (fromList x) cl new)
getRow (x:xs) rw cl new = getRow xs (rw-1) cl new

getColumn board cl new = update cl new $ board
