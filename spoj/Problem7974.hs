module Main where

import Control.Monad

solve :: String -> IO ()
solve nStr = do
    let (a:b:c:_) = map (read) (words nStr) :: [Integer]
    if c - b == b - a
        then putStrLn ("AP " ++ show (c + (c - b)))
        else putStrLn ("GP " ++ show (c * (c `div` b)))

main :: IO ()
main = do
    nStr <- getLine
    if nStr /= "0 0 0"
        then solve nStr >> main
        else return ()