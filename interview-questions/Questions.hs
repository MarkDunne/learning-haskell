module Main where

import Data.List
import qualified Data.Map
import Control.Monad

--Question 1 You are given a list of primes. 
--Output a list of all possible products of them. 
--For example, if given [2, 3, 11] output [1, 2, 3, 6, 11, 22, 33, 66].
--Source: http://sahandsaba.com/interview-question-facebook-primes.html

--powerset :: [a] -> [[a]]
--powerset = filterM (const [True, False])

--build powerset by adding new elems to elems already in powerset
--start with an empty list
powerset :: [a] -> [[a]]
powerset = foldl f [[]]
	where f list newElem = list ++ [newElem:powerElem | powerElem <- list]

solution1 = map product (powerset [2,3,11])

--Question 2 
--Given a list of words, group them into anagrams. 
--For example given ["tsar", "rat", "tar", "star", "tars", "cheese"] 
--output [["tsar", "star", "tars"],["rat", "tar"],["cheese"]]
--the order in the output does not matter.
--Source: http://sahandsaba.com/interview-question-facebook-anagrams.html

groupEqual :: Ord a => [a] -> [[a]]
groupEqual list = groupEqualBy id list 

groupEqualBy :: Ord k => (a -> k) -> [a] -> [[a]]
groupEqualBy hasher = Data.Map.elems . foldl f Data.Map.empty
	where f m elem = Data.Map.insertWith (++) (hasher elem) [elem] m

solution2 = groupEqualBy sort ["tsar", "rat", "tar", "star", "tars", "cheese"]

main = print $ groupEqual [1]
