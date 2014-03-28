module Main where

import Control.Monad

solve :: (Integral a) => a -> a
solve n = n * (n + 1) * (2 * n + 1) `div` 6
main :: IO ()
main = do
    nStr <- getLine
    let n = read nStr
    if n /= 0
        then (print . solve $ n ) >> main
        else return ()