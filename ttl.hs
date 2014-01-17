import Control.Monad
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


toTile '.' = Empty
toTile 'X' = X
toTile 'O' = O
toTile p = error [p]

opposite X = O
opposite O = X
opposite _ = error "Only X and O have opposites"
-- UPDATE ARRAY let newArr = oldArr (\\) [((i,j),val)]

inBounds a (x,y)
    | x < 0 || y < 0 = False
    | otherwise =
    let (_, (i,j)) = bounds a
    in x <= i && y <= j

checkLeft a (i,j)
    | inBounds a (i, j-2) && a ! (i, j-1) == a ! (i, j-2) = a ! (i, j-1)
    | otherwise = Empty
checkRight a (i,j)
    | inBounds a (i, j+2) && a ! (i, j+1) == a ! (i, j+2) = a ! (i, j+1)
    | otherwise = Empty
checkUp a (i,j)
    | inBounds a (i-2, j) && a ! (i -1, j) == a ! (i-2, j) = a ! (i-1, j)
    | otherwise = Empty
checkDown a (i,j)
    | inBounds a (i+2, j) && a ! (i+1, j) == a ! (i+2, j)  = a ! (i+1, j)
    | otherwise = Empty

 -- Checks to see if neighbours have two elements appearing after eachother Maybe Int
neighbourToPair a (i, j)
    | c1 /= Empty = opposite c1
    | c2 /= Empty = opposite c2
    | c3 /= Empty = opposite c3
    | c4 /= Empty = opposite c4
    | otherwise = Empty
    where
        c1 = checkLeft a (i, j)
        c2 = checkRight a (i, j)
        c3 = checkUp a (i, j)
        c4 = checkDown a (i, j)

elemFilled m (i, j) = False -- Does row/col contain n/2 of any element?

--inBetweenRow m (i, j) =
inBetween m (i, j) = False -- Is element in between 2 elements of the same value?

guess = False -- Randomly picks an element and dfs that shizz

solve m =
    False -- Solves binero

solver = False


makeArray n m l = array ((0,0),(n,m)) l --[((i,j),k) | i<-[0..n], j<-[0..m], k<-[1,2,3,4]]

-- array ((0,0),(n,m)) [((i,j),Empty) | i<-[0..n], j<-[0..m]]
-- Validate solution
--validate :: Matrix -> Bool
validate m = False

scanLine i j [] = []
scanLine i j (h:xs) = (((i,j), toTile h):(scanLine i (j+1) xs))

--getLineN :: IO [((Int, Int),Tile)]
getLineN rows n
    | rows == n = return []
    | otherwise = do
        l <- getLine
        next <- (getLineN rows (n+1))
        return $ (scanLine n 0 l) ++ next

readWords :: (Read a) => String -> [a]
readWords = map read . words
lineToReadInt :: IO[Int]
lineToReadInt = (readWords) `fmap` getLine
--readLine = scanLine 0 `fmap` getLine

main = do
    ln <- lineToReadInt
    let rws = ln !! 0
        cls = ln !! 1
    board <- getLineN rws 0
    let arr = makeArray (rws-1) (cls-1) board
        --b = arr // [((i,j), c) | i<-[0..(rws-1)], j<-[0..(cls-1)], let c = (ntp arr (i,j)), c /= Empty] -- Updates the array
        b = [((i,j), c) | i<-[0..(rws-1)], j<-[0..(cls-1)], let c = (neighbourToPair arr (i,j)), c /= Empty] -- Just to see which ones are updated
    print b
