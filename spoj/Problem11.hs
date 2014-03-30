module Main where

import Control.Monad

solveInput = do
    input <- getLine
    let n = read input
        parts = [floor $ n / (5 ** i) | i <- [1..]]
    print $ sum $ takeWhile (1<=) parts

main = do
    numCases <- getLine
    sequence_ $ replicate (read numCases) solveInput