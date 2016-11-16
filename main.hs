import System.IO


-- Cell data
type Cell = (Char, Int) -- represents a single Cell in the Matrix

emptyCell :: Cell    -- cell that hasn't been scored yet
emptyCell = ('0', 0)

leftCell :: Int -> Cell     -- left pointing cell
leftCell score = ('-', score)

diagonalCell :: Int -> Cell     -- up-left pointing cell
diagonalCell score = ('\\', score)

upCell :: Int -> Cell   -- upwards pointing cell
upCell score = ('|', score)


-- Matrix data
type Matrix = [[Cell]]  -- represents a 2D grid of Cells

emptyMatrix :: Int -> Int -> Matrix
emptyMatrix rows cols = [[emptyCell | _ <- [0..cols]] | _ <- [0..rows]]

setCellInRow :: Int -> Cell -> [Cell] -> [Cell]
setCellInRow col val (a:as)
    | col == 0 = val:as
    | otherwise = a:(setCellInRow (col - 1) val as)

setCell :: Int -> Int -> Cell -> Matrix -> Matrix
setCell row col val ((a:as) : (b:bs))
    | row < 0 || col < 0 = ((a:as) : (b:bs))
    | row == 0 = (setCellInRow col val (a:as)):(b:bs)
    | otherwise = (a:as):(setCell (row - 1) col val (b:bs))


-- Algorithm
smithWaterman :: String -> String -> [String]
smithWaterman seq1 seq2 = []


-- Main function
main = do fromHandle <- openFile "test_input.txt" ReadMode
          contents   <- hGetContents fromHandle
          let [header1, seq1, header2, seq2] = lines contents
          let result = smithWaterman seq1 seq2
          putStrLn "\n## Local Aligner"
          putStrLn contents

