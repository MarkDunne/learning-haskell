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

average :: (Num a, Fractional a) => [a] -> a
average ls = sum ls / genericLength ls

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
          weight column = (binaryEntropy column) * genericLength column

main = do
    print $ [columnEntropy testData col | col <- [0..3]]

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