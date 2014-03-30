module Main where

import Control.Monad
import qualified Data.Vector as V

solve :: Int -> IO ()
solve n = do
    permStr <- getLine
    let perm = map read (words permStr) :: [Int]
        vect = V.fromList perm
        newPerm = V.map ((vect V.!) . subtract 1) vect
        isSorted = V.and $ V.zipWith (<=) newPerm (V.tail newPerm)
    if isSorted
        then putStrLn "ambiguous"
        else putStrLn "not ambiguous"

main :: IO ()
main = do
    nStr <- getLine
    let n = read nStr :: Int
    if n /= 0
        then solve n >> main
        else return ()