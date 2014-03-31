module Main where

import Control.Monad
import Data.List

sumMod :: Integral a => a -> [a] -> a
sumMod n = foldr f 0
	where f a b = mod (a + b) n

solveLine = do
    line <- getLine
    let (n:k:l:_) = map read (words line)
    	fibs = [0.. n-1] ++ map (sumMod (10^k)) (transpose [drop i fibs | i <- [0.. n-1]])
    print $ fibs !! l

main = do
    numCases <- getLine
    sequence_ $ replicate (read numCases) solveLine