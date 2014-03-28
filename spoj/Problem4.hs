module Main where

import Control.Monad

nextPalindrome :: String -> String
nextPalindrome numStr = halfStr ++ drop p (reverse halfStr)
    where len = length numStr
          (d, p) = len `quotRem` 2
          halfStr = show . (+1) . read . take (d + p) $ numStr

main = do
    numCases <- getLine
    inputs <- replicateM (read numCases) getLine
    mapM_ (putStrLn . nextPalindrome) inputs