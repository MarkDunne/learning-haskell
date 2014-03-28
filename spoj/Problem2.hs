module Main where

import Control.Monad

primes :: [Integer]
primes = 2 : 3 : [x+i | x <- [6,12..], i <- [-1, 1], all (relativelyPrime $ x+i) (candidateDivisors $ x+i)]
    where relativelyPrime p n = not $ p `mod` n == 0
          candidateDivisors p = takeWhile (\n -> n*n <= p) primes

primeR :: Integer -> Integer -> [Integer]
primeR lo hi = takeWhile (<=hi) $ dropWhile (<lo) primes

--solve :: String -> [Integer]
solve input = do
    let m = read (input !! 0) :: Integer
        n = read (input !! 1) :: Integer
        solution = primeR m n
    mapM_ print solution
    putStrLn ""

main = do
    numCases <- getLine
    inputs <- replicateM (read numCases) getLine
    mapM_ (solve . words) inputs