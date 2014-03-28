module Main where

import Control.Monad

solve :: (Fractional a) => a -> a
solve n = 
    let nPlus = n + 1
    in(3 * nPlus ^ 2 - nPlus) / 2

main :: IO ()
main = do
    nStr <- getLine
    let n = read nStr
    if n /= 0
        then (print . solve $ n ) >> main
        else return ()