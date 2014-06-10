-- For GS Code Golf - Numbers Game
-- Read Doubles from input, output min, max, average and variance
-- Solution using State monad
-- Runs in about 100 ms on 1.5k doubles

module Main where

    import Control.Monad.State

    data ProgramVariables = ProgramVariables {
        minVar :: Double,
        maxVar :: Double,
        sumVar :: Double,
        sumSqVar :: Double,
        counterVar :: Double
    }

    updateState :: ProgramVariables -> Double -> ProgramVariables
    updateState state n = 
        ProgramVariables {
            minVar = min (minVar state) n,
            maxVar = max (maxVar state) n,
            sumVar = sumVar state + n,
            sumSqVar = sumSqVar state + n * n,
            counterVar = counterVar state + 1
        }

    data Solution = Solution{
        minFinal :: Double,
        maxFinal :: Double,
        average :: Double, 
        variance :: Double
    } deriving Show

    runProgram :: [String] -> State ProgramVariables Solution
    runProgram [] = do
        ProgramVariables minVar maxVar sumVar sumSqVar counter <- get
        return $ Solution {
            minFinal = minVar,
            maxFinal = maxVar,
            average = sumVar / counter,
            variance = (sumSqVar - (sumVar * sumVar / counter)) / (counter - 1)
        }

    runProgram (x:xs) = do
        let num = read x :: Double
        currentState <- get
        put (updateState currentState num)
        runProgram xs

    main = do
        rawInput <- getLine
        let input = words rawInput
            startingState = ProgramVariables 0 0 0 0 0
        print $ evalState (runProgram input) startingState
