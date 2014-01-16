import Data.Array
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



--type Matrix = Array (Int,Int)
checkLeft m (i,j)
    | j < 1 = False
    | otherwise = (m ! (i, (j-1))) == (m ! (i, (j-2)))

neighbourToPair m (i, j) = False -- Checks to see if neighbours have two elements appearing after eachother Maybe Int

elemFilled m (i, j) = False -- Does row/col contain n/2 of any element?

inBetween m (i, j) = False -- Is element in between 2 elements of the same value?

guess = False -- Randomly picks an element and dfs that shizz

solve m =
    False -- Solves binero

solver = False

makeArray n m = array ((0,0),(n,m)) [((i,j),Empty) | i<-[0..n], j<-[0..m]]
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
