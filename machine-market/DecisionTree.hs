module DecisionTree where

	import Data.List
	import Debug.Trace

	entropy :: [Bool] -> Double
	entropy outcomes = calc probTrue + calc (1 - probTrue)
		where probTrue = genericLength (filter id outcomes) / genericLength outcomes
			  calc 1 = 0
			  calc 0 = 0
			  calc prob = -prob * logBase 2 prob

	tableEntropy :: ([Bool], [Bool]) -> Double
	tableEntropy (c1, c2) = calc c1 + calc c2
		where coeff c = genericLength c / (genericLength c1 + genericLength c2)
			  calc c = coeff c * entropy c
			  
	infoGain :: [Bool] -> [Bool] -> Double
	infoGain inputs outcomes = entropy outcomes - tableEntropy table
		where table = foldl makeTable ([], []) (zip inputs outcomes)
			  makeTable (trueCol, falseCol) (input, outcome)
				  | outcome = (input:trueCol, falseCol)
				  | otherwise = (trueCol, input: falseCol)
			  
	main = do
		print $ infoGain [True, False, True] [True, False, True]
