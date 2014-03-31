module Main where

import Control.Monad

--Use Shuting Yard Algorithm described here
--http://en.wikipedia.org/wiki/Shunting-yard_algorithm

permute :: [a] -> [Int] -> [a]
permute ls perm = map (ls !!) perm

isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/^"

transform :: [Char] -> [Char]-> [Char]
transform (c:[]) opStack = opStack
transform (c:cs) opStack
    | c == '(' = transform cs opStack
    | c == ')' = (head opStack) : transform cs (tail opStack)
    | isOperator c = transform cs (c : opStack)
    | otherwise = c : transform cs opStack

solveLine = do
    expression <- getLine
    putStrLn $ transform expression []

main = do
    numCases <- getLine
    sequence_ $ replicate (read numCases) solveLine