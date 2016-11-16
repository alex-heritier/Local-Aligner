type Cell = (Char, Int) -- represents a single Cell in the Matrix

zeroCell :: Cell
zeroCell = ('0', 0)

leftCell :: Int -> Cell
leftCell score = ('-', score)

diagonalCell :: Int -> Cell
diagonalCell score = ('\\', score)

upCell :: Int -> Cell
upCell score = ('|', score)

type Matrix = [[Cell]]

setCellInRow :: Int -> Cell -> [Cell] -> [Cell]
setCellInRow col val (a:as)
    | col == 0 = val:as
    | otherwise = a:(setCellInRow (col - 1) val as)

setCell :: Int -> Int -> Cell -> Matrix -> Matrix
setCell row col val ((a:as) : (b:bs))
    | row < 0 || col < 0 = ((a:as) : (b:bs))
    | row == 0 = (setCellInRow col val (a:as)):(b:bs)
    | otherwise = (a:as):(setCell (row - 1) col val (b:bs))