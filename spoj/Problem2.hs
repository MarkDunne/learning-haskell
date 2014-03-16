module Main where

import Control.Monad

doSum :: String -> Int
doSum input = read $ reverse (show finalSum)
    where finalSum = sum intList
          intList = map read firstRev
          firstRev = (words . reverse) input

main = do
    cases <- getLine
    inputs <- replicateM (read cases) getLine
    mapM_ print (map doSum inputs)