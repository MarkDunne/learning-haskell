module Main where

import Control.Monad
import qualified Data.Set as Set

extractFromLine :: String -> (Set.Set String, Set.Set String)
extractFromLine line = (Set.singleton friend, Set.fromList friendsOfFriends)
    where (friend:num:friendsOfFriends) = words line

solve :: [String] -> Int
solve lines = Set.size $ Set.difference friendsOfFriends friends
    where (friends, friendsOfFriends) = Prelude.foldl f (Set.empty, Set.empty) (map extractFromLine lines)
          f (a, b) (a', b') = (Set.union a a', Set.union b b')

main = do
    numCases <- getLine
    inputs <- replicateM (read numCases) getLine
    print . solve $ inputs