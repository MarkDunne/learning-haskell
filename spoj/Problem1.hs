module Main where

import Control.Monad

main = do
	input <- getLine
	if input /= "42"
		then putStrLn input >> main
		else return ()