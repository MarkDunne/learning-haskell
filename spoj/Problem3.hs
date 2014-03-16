module Main where

import Control.Monad

solve :: Integer -> Integer
solve n = product [1..n]

main = do
	cases <- getLine
	inputs <- replicateM (read cases) getLine
	mapM_ (print . solve . read) inputs