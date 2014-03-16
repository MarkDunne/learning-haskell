module Main where

import Control.Monad

nextPalindrome :: String -> String
nextPalindrome numStr = halfStr ++ drop parity (reverse halfStr)
    where len = length numStr
          parity = len `mod` 2
          halfStr = show . (+1) . read . take (parity + len `div` 2) $ numStr

main = do
    numCases <- getLine
    inputs <- replicateM (read numCases) getLine
    mapM_ (putStrLn . nextPalindrome) inputs