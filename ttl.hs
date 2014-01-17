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

-- Checks to see if neighbours have two elements appearing after eachother Maybe Int
neighbourToPair a (i,j)
    | inBounds a (i, j-2) && left /= Empty && left == a ! (i, j-2) = opposite left
    | inBounds a (i, j+2) && right /= Empty && right == a ! (i, j+2) = opposite right
    | inBounds a (i-2, j) && up == a ! (i-2, j) = opposite up
    | inBounds a (i+2, j) && down == a ! (i+2, j) = opposite down
    | otherwise = Empty
    where
        left = a ! (i, j-1)
        right = a ! (i, j+1)
        up = a ! (i -1, j)
        down = a ! (i +1, j)

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

simpleSolve arr rws cls =
    let new = arr //[((i,j), c) | i<-[0..(rws-1)], j<-[0..(cls-1)], let c = (neighbourToPair arr (i,j)), c /= Empty]
    in new

-- http://www.markhneedham.com/blog/2012/04/03/haskell-print-friendly-representation-of-an-array/
printGrid :: Show a => Array (Int, Int) a -> IO [()]
printGrid grid = sequence $ map (putStrLn . textRepresentation) $ toSimpleArray grid

toSimpleArray :: Array (Int, Int) a -> [[a]]
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]]
    where ((lowx, lowy), (highx, highy)) =  bounds grid

textRepresentation :: Show a => [a] -> String
textRepresentation row = foldl (\acc y -> acc ++ (show y)) "" row

main = do
    ln <- lineToReadInt
    let rws = ln !! 0
        cls = ln !! 1
    board <- getLineN rws 0
    let arr = makeArray (rws-1) (cls-1) board
        b = simpleSolve arr rws cls
    print "INITIAL"
    printGrid arr
    print "-----------"
    print "1 Iteration"
    printGrid b

