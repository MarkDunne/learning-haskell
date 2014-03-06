module DecisionTree where

import Data.Ord
import Data.List
import Debug.Trace
import qualified Data.Map as Map

data Row a = Row [a] Bool
  deriving (Show)

data Table a = Table {
    rows :: [Row a]
} deriving (Show)

data DecisionTree a = DecisionLeaf (Table a) | DecisionBranch {
  decisionTable :: Table a,
  subDecisions :: Map.Map a (DecisionTree a),
  decisionName :: String
} deriving (Show)

numAttributes :: Table a -> Int
numAttributes (Table rows) = numAttributesRow $ head rows
  where numAttributesRow (Row attributes outcome) = length attributes 

average :: (Num a, Fractional a) => [a] -> a
average ls = sum ls / genericLength ls

removeNth :: [a] -> Int -> [a]
removeNth ls n = map (\(x, _) -> x) . filter (\(_, num) -> num /= n) $ zip ls [0..]

addRowToTable :: Table a -> Row a -> Table a
addRowToTable (Table rows) row = Table (row:rows)

binaryEntropy :: [Bool] -> Double
binaryEntropy inputs = calc probTrue + calc (1 - probTrue)
    where probTrue = genericLength (filter id inputs) / genericLength inputs
          calc 0 = 0
          calc 1 = 0
          calc prob = -prob * logBase 2 prob

splitColumn :: Ord k => Table k -> Int -> Map.Map k [Bool]
splitColumn (Table rows) colNum = foldl f Map.empty rows
    where f m (Row attributes outcome) = Map.insertWith (++) (attributes !! colNum) [outcome] m

columnEntropy :: Ord k => Table k -> Int -> Double
columnEntropy table attributeCol = weightedAverage (Map.elems (splitColumn table attributeCol))
    where weightedAverage columns = (sum $ map weight columns) / (genericLength $ rows table)
          weight column = binaryEntropy column * genericLength column

splitTable :: Ord k => Table k -> Int -> Map.Map k (Table k)
splitTable (Table rows) colNum = Map.map (\rows -> Table rows) $ foldl f Map.empty rows
    where f m (Row attributes outcome) = Map.insertWith (++) (attributes !! colNum) [Row (newAttributes attributes) outcome] m
          newAttributes attributes = removeNth attributes colNum

--buildTree :: Table a -> DecisionTree a
buildTree :: Ord a => DecisionTree a -> (Int -> String) -> DecisionTree a
buildTree (DecisionBranch table _ name) namingFunc = DecisionBranch table subDecisions decisionName
  where nextDecision = minimumBy (comparing $ columnEntropy table) [0.. numAttributes table - 1]
        subDecisions = Map.map (\table -> DecisionLeaf table) $ splitTable table nextDecision
        decisionName = namingFunc nextDecision

main = do
  print $ buildTree (DecisionBranch testData Map.empty "start") columnNameFuncTest

columnNameFuncTest :: Int -> String
columnNameFuncTest 0 = "Outlook"
columnNameFuncTest 1 = "Temperature"
columnNameFuncTest 2 = "Humidity"
columnNameFuncTest 3 = "Windy"

testData = Table [Row ["sunny", "hot", "high", "false"] False,
                  Row ["sunny", "hot", "high", "true"] False,
                  Row ["overcast", "hot", "high", "false"] True,
                  Row ["rain", "mild", "high", "false"] True,
                  Row ["rain", "cool", "normal", "false"] True,
                  Row ["rain", "cool", "normal", "true"] False,
                  Row ["overcast", "cool", "normal", "true"] True,
                  Row ["sunny", "mild", "high", "false"] False,
                  Row ["sunny", "cool", "normal", "false"] True,
                  Row ["rain", "mild", "normal", "false"] True,
                  Row ["sunny", "mild", "normal", "true"] True,
                  Row ["overcast", "mild", "high", "true"] True,
                  Row ["overcast", "hot", "normal", "false"] True,
                  Row ["rain", "mild", "high", "true"] False]