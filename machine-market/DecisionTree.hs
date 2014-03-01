module DecisionTree where

import Data.List
import Debug.Trace

data Column a = Column [a]
  deriving (Show)

data Table a = Table {
  attributes :: [Column a],
  outcomes :: Column Bool
} deriving (Show)

entropy :: Table a -> Double
entropy (Table _ (Column outcomes)) = calc probTrue + calc (1 - probTrue)
    where probTrue = genericLength (filter id outcomes) / genericLength outcomes
          calc 0 = 0
          calc 1 = 0
          calc prob = -prob * logBase 2 prob

--splitColumn :: Table -> Int -> (Table, Table)
--splitColumn (Table cols (BoolColumn) = (a, a)

--entropy (StrColumn event) = map entropy (splitColumn)

--tableEntropy :: ([Bool], [Bool]) -> Double
--tableEntropy (c1, c2) = calc c1 + calc c2
--    where coeff c = genericLength c / (genericLength c1 + genericLength c2)
--          calc c = coeff c * entropy c
          
--infoGain :: Ord a => [a] -> [Bool] -> Double
--infoGain inputs outcomes = entropy outcomes - tableEntropy table
--    where table = foldl makeTable ([], []) (zip inputs outcomes)
--          makeTable (trueCol, falseCol) (input, outcome)
--              | outcome = (input:trueCol, falseCol)
--              | otherwise = (trueCol, input: falseCol)
          
main = do
    print $ Table [Column [1]] (Column [True])



--infoGain ["sunny","sunny","overcast","rain","rain","rain","overcast","sunny","sunny","rain","sunny","overcast","overcast","rain"] [False, False, True, True, True, False, True, False, True, True, True, True, True, False]