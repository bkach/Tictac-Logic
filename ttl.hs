import Parser
import Data.Array

type Position = (Int, Int)

{-type Matrix = Array (Int,Int)-}

{-inBounds a (x,y)-}
    {-| x < 0 || y < 0 = False-}
    {-| otherwise =-}
    {-let (_, (i,j)) = bounds a-}
    {-in x <= i && y <= j-}

inBounds (rws,cls,board) (x,y)
    | x < 0 || y < 0 = False
    | x >= rws || y >= cls = False
    | otherwise = True

{-checkLeft a (i,j)-}
    {-| inBounds a (i, j-2) = (a ! (i, (j-1))) == (a ! (i, (j-2)))-}
    {-| otherwise = False-}
checkLeft board (i,j)
    | inBounds board (i, j-2) = (readTile board (i, j-1)) == (readTile board (i, j-2))
    | otherwise = False

{-checkRight a (i,j)-}
    {-| inBounds a (i, j+2) = (a ! (i, (j+1))) == (a ! (i, (j+2)))-}
    {-| otherwise = False-}
checkRight board (i,j)
    | inBounds board (i, j+2) = (readTile board (i, j+1)) == (readTile board (i, j+2))
    | otherwise = False

{-checkUp a (i,j)-}
    {-| inBounds a (i-2, j) = (a ! ((i -1), j)) == (a ! ((i-2), j))-}
    {-| otherwise = False-}
checkUp board (i,j)
    | inBounds board (i-2, j) = (readTile board (i-1, j)) == (readTile board (i-2, j))
    | otherwise = False

{-checkDown a (i,j)-}
    {-| inBounds a (i+2, j) = (a ! ((i+1), j)) == (a ! ((i+2), j))-}
    {-| otherwise = False-}
checkDown board (i,j)
    | inBounds board (i+2, j) = (readTile board (i+1, j)) == (readTile board (i+2, j))
    | otherwise = False

neighbourToPair arr (i, j) = False -- Checks to see if neighbours have two elements appearing after eachother Maybe Int
   

elemFilled m (i, j) = False -- Does row/col contain n/2 of any element?

--inBetweenRow m (i, j) =
inBetween m (i, j) = False -- Is element in between 2 elements of the same value?

guess = False -- Randomly picks an element and dfs that shizz

solve m =
    False -- Solves binero

solver = False


{-makeArray n m = do-}
        {-arr <- newArray ((0,0),(9,9)) Empty :: ST s (STArray s (Int,Int) Tile)-}
        {-return arr-}

-- array ((0,0),(n,m)) [((i,j),Empty) | i<-[0..n], j<-[0..m]]
-- Validate solution
--validate :: Matrix -> Bool
validate m = False

lineToRead :: IO[Int]
lineToRead = (map read . words) `fmap` getLine

main :: IO ()
main = do
    board <- getBoard
    print board
