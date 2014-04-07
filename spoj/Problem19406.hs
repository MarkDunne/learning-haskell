module Main where

import Control.Monad

sq5 :: Double
sq5 = sqrt 5 

phi :: Double
phi = (1 + sq5) / 2

fib :: Integer -> Integer 
fib n = round $ phi ** fromIntegral n / sq5

sol :: Integer -> Integer -> Integer -> Integer
sol c k n = sum [fib (k * i + c) | i <- [1..n]]

solveLine = do
    line <- getLine
    let (c:k:n:_) = map read (words line)
    print $ sol c k n

main = do
    numCases <- getLine
    sequence_ $ replicate (read numCases) solveLine