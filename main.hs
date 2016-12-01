-- Authors: Alex Heritier, Wilson Loi
-- Date: 11/10/16
--
-- This program is an implementation of the Smith-Waterman local
-- alignment algorithm in Haskell.
--
-- To run this program, you first need the Glasgow Haskell
-- Compiler (ghc), found here https://www.haskell.org/ghc/.
-- Once installed, run "ghc parallel_main.hs" to compile the
-- program and then run "./parallel_main <input file>" with
-- <input file> being a fasta file with 4 lines. The first and
-- third lines must be description lines while the second and
-- fourth lines must be the sequences that will be compared.


import System.IO
import System.Environment
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

emptyMatrix :: Int -> Int -> Matrix -- matrix full of directionless, scoreless Cells
emptyMatrix rows cols = [[emptyCell | _ <- [0..cols-1]] | _ <- [0..rows-1]]

_setCellInRow :: Int -> Cell -> [Cell] -> [Cell]
_setCellInRow col val (a:as)
    | col == 0 = val:as
    | otherwise = a:(_setCellInRow (col - 1) val as)

setCell :: Int -> Int -> Cell -> Matrix -> Matrix -- change a Cell in the Matrix
setCell row col val [(a:as)]
    | row == 0 = [_setCellInRow col val (a:as)]
    | otherwise = [(a:as)]
setCell row col val ((a:as) : (b:bs))
    | row < 0 = ((a:as) : (b:bs))
    | row == 0 = newRow:(b:bs)
    | otherwise = (a:as):(setCell (row - 1) col val (b:bs))
    where newRow = _setCellInRow col val (a:as)

getCell :: Int -> Int -> Matrix -> Cell -- get a Cell in the Matrix
getCell row col m
    | row < 0 || col < 0 = emptyCell
    | row == length m || col == length (head m) = error "getCell: Index out of bounds"
    | otherwise = (m !! row) !! col


type Coordinate = (Int, Int) -- a Cell's coordinate in the Matrix

findCellCoordinates :: Int -> Int -> Int -> Matrix -> Coordinate -- get a Cell's coordinates
findCellCoordinates row col val m
    | row == length m = error "findCellCoordinates: value does not exist"
    | col == (length $ head m) = findCellCoordinates (row + 1) 0 val m
    | (snd currentCell) == val = (row, col)
    | otherwise = findCellCoordinates row (col + 1) val m
    where currentCell = getCell row col m

-- Algorithm
_mapMatrix :: Int -> Int -> String -> String -> Matrix -> Matrix
_mapMatrix rows cols seq1 seq2 m
    | rows == length m = m -- base case
    | cols == (length $ head m) = _mapMatrix (rows + 1) 0 seq1 seq2 m -- move to next row, no changes
    | otherwise = _mapMatrix rows (cols + 1) seq1 seq2 newMatrix
    where newMatrix = setCell rows cols newCell m
          newCell = scoreCell m seq1 seq2 rows cols

mapMatrix :: String -> String -> Matrix -> Matrix -- loop through the Matrix, scoring each Cell
mapMatrix s1 s2 m = _mapMatrix 0 0 s1 s2 m

getMaxCellInList :: [Cell] -> Cell -- get the highest score in the Matrix
getMaxCellInList list = foldr (\x y -> if ((snd x) > (snd y)) then (x) else (y)) (('_', -100)) list

getMaxScoreCellCoordinates :: [Coordinate] -> Matrix -> Coordinate -- get the coordinates for the Cell with the highest score
getMaxScoreCellCoordinates res m =
    let getMaxScoreCell = getMaxCellInList (map getMaxCellInList m)
    in findCellCoordinates 0 0 (snd getMaxScoreCell) m

type Bases = (Char, Char) -- the two bases that intersect at a Cell
type ResultCell = (Cell, Bases) -- A Cell and then the bases that intersect on it

_getResultChain :: [ResultCell] -> Int -> Int -> Matrix -> String -> String -> [ResultCell]
_getResultChain res row col m seq1 seq2
    | row < 0 || col < 0 = res
    | direction == '-' = _getResultChain ((currentCell, bases):res) row (col - 1) m seq1 seq2
    | direction == 'd' = _getResultChain ((currentCell, bases):res) (row - 1) (col - 1) m seq1 seq2
    | direction == '|' = _getResultChain ((currentCell, bases):res) (row - 1) col m seq1 seq2
    | otherwise = error "_getResultChain: Bad direction"
    where direction = fst currentCell
          currentCell = getCell row col m
          lc = getCell row (col - 1) m
          dc = getCell (row  - 1) (col - 1) m
          uc = getCell (row - 1) col m
          bases = (seq1 !! row, seq2 !! col)

getResultChain :: Matrix -> String -> String -> [ResultCell] -- get the Cell chain starting from the highest scoring Cell to a border Cell
getResultChain m seq1 seq2 =
    let maxScoreCellCoordinates = getMaxScoreCellCoordinates [] m
        maxRow = fst maxScoreCellCoordinates
        maxCol = snd maxScoreCellCoordinates
    in _getResultChain [] maxRow maxCol m seq1 seq2

alignPair :: ResultCell -> String -- do an alignment for a single pair of bases based off the ResultCell data
alignPair rc
    | cellType == 'd' = [fst bases, snd bases]
    | cellType == '|' = ['_', snd bases]
    | cellType == '-' = [fst bases, '_']
    | otherwise = error "alignPair: bad cellType"
    where bases = snd rc
          cell = fst rc
          cellType = fst cell

_getAlignment :: [String] -> [ResultCell] -> [String]
_getAlignment res [x] =
    let alignment = alignPair x
    in [(res !! 0) ++ [alignment !! 0], (res !! 1) ++ [alignment !! 1]]
_getAlignment res (a:as) =
    let alignment = alignPair a
        newResult = [(res !! 0) ++ [alignment !! 0], (res !! 1) ++ [alignment !! 1]]
    in _getAlignment newResult as

getAlignment :: [ResultCell] -> [String] -- create the alignment from a result Cell chain
getAlignment rc = _getAlignment ["", ""] rc

scoreCell :: Matrix -> String -> String -> Int -> Int -> Cell -- scores a Cell
scoreCell m seq1 seq2 i j
    | match > delete && match > insert = (diagonalCell match)
    | delete > match && delete > insert = (upCell delete)
    | insert > match && insert > delete = (leftCell insert)
    | otherwise = (diagonalCell match)
    where lc = getCell i (j - 1) m -- left cell
          dc = getCell (i - 1) (j - 1) m -- diagonal cell
          uc = getCell (i - 1) j m -- up cell
          lcScore = snd lc
          dcScore = snd dc
          ucScore = snd uc
          match = dcScore + (matchScore b1 b2)
          delete = ucScore + gapPenalty
          insert = lcScore + gapPenalty
          b1 = seq1 !! i -- sequence 1 base
          b2 = seq2 !! j -- sequence 2 base

gapPenalty :: Int -- the gap penalty
gapPenalty = -1

matchScore :: Char -> Char -> Int -- the match/mismatch penalty
matchScore b1 b2
    | b1 == b2 = 2
    | otherwise = -1

smithWaterman :: String -> String -> [String] -- perform a Smith-Waterman alignment on two sequences
smithWaterman seq1 seq2 = getAlignment $ getResultChain (mapMatrix seq1 seq2 (emptyMatrix (length seq1) (length seq2))) seq1 seq2


-- Main function
main = do args <- getArgs
          fromHandle <- openFile (if (length args == 1) then (args !! 0) else (error "usage: ghci parallel_main.hs <input file>")) ReadMode
          contents   <- hGetContents fromHandle
          let [header1, seq1, header2, seq2] = lines contents
          let result = smithWaterman seq1 seq2
          putStrLn "\n## Local Aligner"
          putStrLn contents
          putStrLn ""
          putStrLn ""
          putStrLn (result !! 0)
          putStrLn (result !! 1)
