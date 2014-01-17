import Control.Monad.ST
import Data.Array
import Data.Array.ST
type Position = (Int, Int)

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

--type Matrix = Array (Int,Int)
opposite X = O
opposite O = X
opposite _ = error "Only X and O have opposites"

inBounds a (x,y)
    | x < 0 || y < 0 = False
    | otherwise =
    let (_, (i,j)) = bounds a
    in x <= i && y <= j

checkLeft a (i,j)
    | inBounds a (i, j-2) = (a ! (i, (j-1))) == (a ! (i, (j-2)))
    | otherwise = False
checkRight a (i,j)
    | inBounds a (i, j+2) = (a ! (i, (j+1))) == (a ! (i, (j+2)))
    | otherwise = False
checkUp a (i,j)
    | inBounds a (i-2, j) = (a ! ((i -1), j)) == (a ! ((i-2), j))
    | otherwise = False
checkDown a (i,j)
    | inBounds a (i+2, j) = (a ! ((i+1), j)) == (a ! ((i+2), j))
    | otherwise = False

neighbourToPair arr (i, j) = -- Checks to see if neighbours have two elements appearing after eachother Maybe Int
    let runSTArray arr

elemFilled m (i, j) = False -- Does row/col contain n/2 of any element?

--inBetweenRow m (i, j) =
inBetween m (i, j) = False -- Is element in between 2 elements of the same value?

guess = False -- Randomly picks an element and dfs that shizz

solve m =
    False -- Solves binero

solver = False


makeArray n m = do
        arr <- newArray ((0,0),(9,9)) Empty :: ST s (STArray s (Int,Int) Tile)
        return arr

-- array ((0,0),(n,m)) [((i,j),Empty) | i<-[0..n], j<-[0..m]]
-- Validate solution
--validate :: Matrix -> Bool
validate m = False

lineToRead :: IO[Int]
lineToRead = (map read . words) `fmap` getLine

main :: IO ()
main = do
    ln <- lineToRead
    let rows = ln !! 0
        cols = ln !! 1
    print cols
