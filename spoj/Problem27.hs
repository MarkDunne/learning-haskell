module Main where

import Control.Monad
import Data.List

solveCase = do
    numAccounts <- getLine
    accounts <- replicateM (read numAccounts) getLine :: IO [String]
    let groups = map accountToStr . group . sort $ accounts
        accountToStr ac = head ac ++ (show . length $ ac)
    mapM_ putStrLn groups
    _ <- getLine
    putStrLn ""

main = do
    numCases <- getLine
    sequence_ $ replicate (read numCases) solveCase