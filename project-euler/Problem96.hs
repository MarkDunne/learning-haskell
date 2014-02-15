module Main where
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Char
import qualified Data.Map as Map

type CellValue = Int
type Position = (Int, Int)
type Sodoku = Map.Map Position CellValue

puzzleData = [ 
    0,0,0,0,0,0,9,0,7,
    0,0,0,4,2,0,1,8,0,
    0,0,0,7,0,5,0,2,6,
    1,0,0,9,0,4,0,0,0,
    0,5,0,0,0,0,0,4,0,
    0,0,0,5,0,7,0,0,9,
    9,2,0,1,0,8,0,0,0,
    0,3,4,0,5,9,0,0,0,
    5,0,7,0,0,0,0,0,0]

testData = [
    3,5,1,2,8,6,4,9,7,
    4,9,2,1,5,7,6,3,8,
    7,8,6,9,3,4,5,1,2,
    2,7,5,4,6,9,1,8,3,
    9,3,8,5,2,1,7,6,4,
    6,1,4,8,7,3,2,5,9,
    8,2,9,6,4,5,3,7,1,
    1,6,3,7,9,2,8,4,5,
    5,4,7,3,1,8,9,2,6]

view :: Sodoku -> IO ()
view sodoku = do
    print $ [valAt sodoku (0, i) | i <- [0..8]]
    print $ [valAt sodoku (1, i) | i <- [0..8]]
    print $ [valAt sodoku (2, i) | i <- [0..8]]
    print $ [valAt sodoku (3, i) | i <- [0..8]]
    print $ [valAt sodoku (4, i) | i <- [0..8]]
    print $ [valAt sodoku (5, i) | i <- [0..8]]
    print $ [valAt sodoku (6, i) | i <- [0..8]]
    print $ [valAt sodoku (7, i) | i <- [0..8]]
    print $ [valAt sodoku (8, i) | i <- [0..8]]

positions :: [Position]
positions = [(i, j) | i <- [0..8], j <- [0..8]]

makePuzzle :: [Int] -> Sodoku
makePuzzle values = Map.fromList $ zip positions values

eliminateMaybe :: Maybe CellValue -> CellValue
eliminateMaybe (Just value) = value
eliminateMaybe Nothing = 0

valAt :: Sodoku -> Position -> CellValue
valAt sodoku pos = eliminateMaybe $ Map.lookup pos sodoku

rowPeers :: Sodoku -> Position -> [CellValue]
rowPeers sodoku (i, j) = [valAt sodoku (i, j) | j <- [0..8]]

colPeers :: Sodoku -> Position -> [CellValue]
colPeers sodoku (i, j) = [valAt sodoku (i, j) | i <- [0..8]]

boxPeers :: Sodoku -> Position -> [CellValue]
boxPeers sodoku (i, j) =
    let boxRow = 3 * (div j 3)
        boxCol = 3 * (div i 3)
    in [valAt sodoku (i, j) | j <- [boxRow..boxRow+2], i <- [boxCol..boxCol+2]]

peers :: Sodoku -> Position -> [CellValue]
peers s p = ((colPeers s p)  `union` (rowPeers s p) `union` (boxPeers s p)) \\ [valAt s p]

place :: Sodoku -> Position -> CellValue -> Sodoku
place sodoku pos value = Map.insert pos value sodoku

definitesPass :: Sodoku -> Sodoku
definitesPass sodoku = 
    let possibilities = [[1..9] \\ peers sodoku pos | pos <- positions]
        possibilitiesAtPos = zip possibilities positions
        numPossibilites (possibs, pos) = length possibs
        definites = filter ((==1) . numPossibilites) $ filter (\(_, pos) -> valAt sodoku pos == 0) possibilitiesAtPos
        new_sodoku = foldl (\sodoku (val:[], pos) -> place sodoku pos val) sodoku definites
    in if length definites > 0
        then new_sodoku
        else sodoku

solve :: Sodoku -> [Position] -> (Bool, Sodoku)
solve sodoku [] = (True, sodoku)
solve sodoku (pos:nextPos)
    | valAt sodoku pos /= 0 = solve sodoku nextPos
    | otherwise = 
        if (length solutions > 0)
            then apply_definites . head $ solutions
            else (False, sodoku)
        where possible_values = [1..9] \\ (peers sodoku pos)
              possible_solutions = [(place sodoku pos val) | val <- possible_values]
              results = [solve solution nextPos | solution <- possible_solutions]
              solutions = filter (\(success, solution) -> success) results
              apply_definites (success, sodoku) = (success, definitesPass sodoku)

--main = do
--    let (success, result) = solve (makePuzzle puzzleData) positions
--    if success
--        then putStrLn "Found solution"
--        else putStrLn "Failed"
--    view result

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n ls = group : groupsOf n rest
    where (group, rest) = splitAt n ls

eulerSolution :: (Bool, Sodoku) -> Integer
eulerSolution (_, sodoku) = sum [(10^(2-i)) * (toInteger $ valAt sodoku (0, i)) | i <- [0..2]]

main = do
    contents <- readFile "sudoku.txt"
    let rawData = filter ((/= 'G') . head) $ lines contents
        gridsStr = groupsOf 9 rawData
        grids = [concat [map digitToInt line | line <- grid] | grid <- gridsStr]
        sodokus = map makePuzzle grids
        solutions = map ((flip solve) positions) sodokus
        eulerSolutions = map eulerSolution solutions
        foo = trace (show eulerSolutions) eulerSolutions
    print $ sum $ foo

--main = print $ eulerSolution (True, makePuzzle testData)