import System.IO
import Debug.Trace


-- Cell data
type Cell = (Char, Int) -- represents a single Cell in the Matrix

emptyCell :: Cell    -- cell that hasn't been scored yet
emptyCell = ('0', 0)

leftCell :: Int -> Cell     -- left pointing cell
leftCell score = ('-', score)

diagonalCell :: Int -> Cell     -- up-left pointing cell
diagonalCell score = ('d', score)

upCell :: Int -> Cell   -- upwards pointing cell
upCell score = ('|', score)


-- Matrix data
type Matrix = [[Cell]]  -- represents a 2D grid of Cells

emptyMatrix :: Int -> Int -> Matrix
emptyMatrix rows cols = [[emptyCell | _ <- [0..cols-1]] | _ <- [0..rows-1]]

_setCellInRow :: Int -> Cell -> [Cell] -> [Cell]
_setCellInRow col val (a:as)
    | col == 0 = val:as
    | otherwise = a:(_setCellInRow (col - 1) val as)

setCell :: Int -> Int -> Cell -> Matrix -> Matrix
setCell row col val [(a:as)]
    | row == 0 = [_setCellInRow col val (a:as)]
    | otherwise = [(a:as)]
setCell row col val ((a:as) : (b:bs))
    | row < 0 || col < 0 = ((a:as) : (b:bs))
    | row == 0 = newRow:(b:bs)
    | otherwise = (a:as):(setCell (row - 1) col val (b:bs))
    where newRow = _setCellInRow col val (a:as)

getCell :: Int -> Int -> Matrix -> Cell
getCell row col m
    | row < 0 || col < 0 = trace ("\nGET CELL\ti: " ++ show row ++ "\tj: " ++ show col) emptyCell
    | row == length m || col == length (head m) = error "getCell: Index out of bounds"
    | otherwise = (m !! row) !! col


-- Algorithm
type ScoringData = (Cell, Cell, Cell, Char, Char)
mapInd :: Int -> Int -> (a -> Int -> b) -> [a] -> [b]
mapInd rows cols f m = zipWith f m [0..]

mapIndInner :: (Int -> Int -> b) -> [a] -> Int -> [b]
mapIndInner f m index = zipWith f (cycle [index]) [0..(length m)-1]

_mapMatrix :: Int -> Int -> (Matrix -> String -> String -> Int -> Int -> Cell) -> String -> String -> Matrix -> Matrix
_mapMatrix rows cols f s1 s2 m = mapInd rows cols (mapIndInner $ scoreCell m s1 s2) m

mapMatrix :: (Matrix -> String -> String -> Int -> Int -> Cell) -> String -> String -> Matrix -> Matrix
mapMatrix f s1 s2 m = _mapMatrix (length m) (length $ head m) f s1 s2 m

getResult :: Matrix -> String -> String -> [String]
getResult m seq1 seq2 = [seq1, seq2]

scoreCell :: Matrix -> String -> String -> Int -> Int -> Cell
scoreCell m s1 s2 i j
    | match > delete && match > insert = (diagonalCell match)
    | delete > match && delete > insert = (upCell delete)
    | insert > match && insert > delete = (leftCell insert)
    where lc = getCell i (j - 1) m -- left cell score
          dc = getCell (i - 1) (j - 1) m -- diagonal cell score
          uc = getCell (i - 1) j m -- up cell score
          lcScore = snd lc
          dcScore = snd dc
          ucScore = snd uc
          match = dcScore + (matchScore b1 b2)
          delete = ucScore + gapPenalty
          insert = lcScore + gapPenalty
          b1 = s1 !! trace ("\nSCORE CELL\ti: " ++ show i ++ "\tj: " ++ show j) i -- sequence 1 base
          b2 = s2 !! j -- sequence 2 base

gapPenalty :: Int
gapPenalty = -2

matchScore :: Char -> Char -> Int
matchScore b1 b2
    | b1 == b2 = 1
    | otherwise = -1

smithWaterman :: String -> String -> [String]
smithWaterman seq1 seq2 = getResult (mapMatrix scoreCell seq1 seq2 (emptyMatrix (length seq1) (length seq2))) seq1 seq2


-- Main function
main = do fromHandle <- openFile "test_input.txt" ReadMode
          contents   <- hGetContents fromHandle
          let [header1, seq1, header2, seq2] = lines contents
          let result = smithWaterman seq1 seq2
          putStrLn "\n## Local Aligner"
          putStrLn contents
          putStrLn ""
          putStrLn ""
          putStrLn (result !! 0)
          putStrLn (result !! 1)

