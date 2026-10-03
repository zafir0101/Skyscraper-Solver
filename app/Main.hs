import qualified Data.Matrix as M
import qualified Data.Vector as V
import Control.Monad (guard)

type Board = M.Matrix Int
type Coordinate = (Int, Int)

data Constraints = Constraints { 
    rowConstraints :: [(Int, Int)],
    columnConstraints :: [(Int, Int)]
} deriving Show

-- Trocar [] por maybe
solvePuzzle :: Board -> Constraints -> Int -> [Board] 
solvePuzzle board constraints maxHeight = solve board (1, 1) 
    where
        size = M.ncols board
        numZeros = size - maxHeight
        
        solve :: Board -> Coordinate -> [Board]
        solve board coordinate = if isFinished coordinate 

            then
            if checkConstraints board then [board]
            else []

            else do
                val <- [0..maxHeight] 
                guard(isValid board coordinate val)
                solve (M.setElem val coordinate board) $ iter coordinate


        isFinished :: Coordinate -> Bool 
        isFinished coordinate = (fst coordinate == -1)

        iter :: Coordinate -> Coordinate
        iter coordinate | (snd coordinate == size && fst coordinate == size) = (-1, -1)
                        | (snd coordinate == size) = (fst coordinate + 1, 1)
                        | otherwise = (fst coordinate, snd coordinate + 1)


        isValid :: Board -> Coordinate -> Int -> Bool
        isValid board coordinate val  =
            -- | (fst coordinate == snd coordinate) =
            --      (checkDiagonal board coordinate val &&
            --      checkRow board coordinate val &&
            --      checkColumn board coordinate val &&
            -- | otherwise =
                (checkRow board coordinate val &&
                checkColumn board coordinate val)

        checkRow :: Board -> Coordinate -> Int -> Bool
        checkRow board coordinate val =
            let row = M.getRow (fst coordinate) board in
            if val /= 0 then
                (V.length (V.filter (\x -> x == val) row) == 0)
            else
               (zeroCount row 1 0 (fst coordinate - 1) <= numZeros)

        checkColumn :: Board -> Coordinate -> Int -> Bool
        checkColumn board coordinate val =
            let column = M.getCol (snd coordinate) board in
            if val /= 0 then
                (V.length (V.filter (\x -> x == val) column) == 0)
            else
               (zeroCount column 1 0 (snd coordinate - 1) <= numZeros)

        checkDiagonal :: Board -> Coordinate -> Int -> Bool
        checkDiagonal board coordinate val =
            let diagonal = M.getDiag board in
            if val /= 0 then
                (V.length (V.filter (\x -> x == val) diagonal) == 0)
            else
               (zeroCount diagonal 1 0 (snd coordinate - 2) <= numZeros)

        checkConstraints :: Board -> Bool
        checkConstraints board  =
            ((foldr checkLineConstraint True (V.zip (matrixToVector board) $ V.fromList $ rowConstraints constraints)) &&
            (foldr checkLineConstraint True (V.zip (matrixToVector (M.transpose board)) $ V.fromList $ columnConstraints constraints)))

        checkLineConstraint :: (V.Vector Int, (Int, Int)) -> Bool -> Bool 
        checkLineConstraint (line, constraint) accum = 
            if (fst constraint == 0 && snd constraint == 0) then accum

            else if (fst constraint == 0) then
                (accum && 
                (snd (foldr greater (-1, 0) line) == snd constraint))

            else if (snd constraint == 0) then
                (accum && 
                (snd (foldr greater (-1, 0) (V.reverse line)) == fst constraint))

            else  
                (accum && 
                (snd (foldr greater (-1, 0) (V.reverse line)) == fst constraint) &&
                (snd (foldr greater (-1, 0) line) == snd constraint))

        greater :: Int -> (Int, Int) -> (Int, Int)
        greater num (max, counter)  | max < num = (num, counter + 1)
                                    | otherwise = (max, counter)

        matrixToVector :: M.Matrix Int -> V.Vector (V.Vector Int)
        matrixToVector m = V.generate (M.nrows m) (\r -> M.getRow (r + 1) m)

        zeroCount :: V.Vector Int -> Int -> Int -> Int -> Int
        zeroCount vector counter current stop
            | current == stop = counter
            | otherwise = 
                if vector V.! current == 0 then zeroCount vector (counter + 1) (current + 1) stop
                else zeroCount vector counter (current + 1) stop

        
initBoard :: Int -> Board
initBoard line = M.zero line line


