module Main where

import Data.List
import Data.Map as Map (elems, empty, insertWith) 
import Control.Monad
import Debug.Trace

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

-- | The 'classify' function takes a list and returns a minimum list of lists such
-- that each sublist in the result contains only equal elements.  For example,
--
-- > classify "Mississippi" = ["M","iiii","pp","ssss"]
--
-- It is a special case of 'classifyBy', which allows the programmer to supply
-- their own equality test.
classify :: Ord a => [a] -> [[a]]
classify = classifyBy id 

-- | The 'classifyBy' function is the non-overloaded version of 'classify'.
classifyBy :: Ord k => (a -> k) -> [a] -> [[a]]
classifyBy hash =  Map.elems . foldl f Map.empty
	where f m elem = Map.insertWith (++) (hash elem) [elem] m

solution2 = classifyBy sort ["tsar", "rat", "tar", "star", "tars", "cheese"]

--Question 3 
--There are 2 non-negative integers: i and j. Given the following equation, 
--find an (optimal) solution to iterate over i and j in such a way that the output is sorted.
--2^i * 5^j
--http://stackoverflow.com/questions/5505894/tricky-google-interview-question

merge :: [Integer] -> [Integer] -> [Integer]
merge (a:as) (b:bs)   
  | a < b   = a : merge as (b:bs)
  | a == b  = a : merge as bs
  | a > b   = b : merge (a:as) bs

solution3 :: [Integer]
solution3 = 1 : merge (map (2*) solution3) (map (5*) solution3)

--Question 4
--FizzBuzz
--Replace multiples of 3 by Fizz, multiples of 5 by Buzz, multiples of 15 by FizzBuzz


fizzBuzz i = if null desc then show i else desc where 
   desc = concat [label | (j,label) <- tags, i `mod` j == 0]
   tags = [(3,"Fizz"), (5,"Buzz")]

main=mapM_ putStrLn [max(show x)(concat[n|(f,n)<-[(3,"Fizz"),(5,"Buzz")],mod x f==0])|x<-[1..100]]

