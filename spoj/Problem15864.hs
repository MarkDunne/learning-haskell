module Main where

merge :: [Integer] -> [Integer] -> [Integer]
merge (a:as) (b:bs) =
    case a `compare` b of 
        LT -> a : merge as (b:bs)
        EQ -> a : merge as bs
        GT -> b : merge (a:as) bs

solutions :: [Integer]
solutions = 1 : merge (map (*2) solutions) (map (*3) solutions)

main = do
    input <- getLine
    let inputs = words input
        x = read $ inputs !! 0
        k = read $ inputs !! 1
    print . (`mod` k) . sum . take x $ solutions