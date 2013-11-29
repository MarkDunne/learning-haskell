--Assignment 1
--Q1
and' :: [Bool] -> Bool
and' xs = foldl (&&) True xs

--Q2
or' :: [Bool] -> Bool
or' xs = foldl (||) False xs

--Q3
issorted :: Ord a => [a] -> Bool
issorted xs = and' $ zipWith (<) xs $ tail xs

--Q4
range :: Integer -> Integer -> [Integer]
range lo hi
	| lo <= hi = lo : range (lo + 1) hi
	| lo > hi = []

--Q5
copies :: Integer -> a -> [a]
copies 0 a = []
copies n a = [a] ++ copies (n-1) a

--Assignment 2
--Q1
applyAll :: [(a -> b)] -> a -> [b]
applyAll fs x = [f x | f <- fs]

--Q2
remove :: (a -> Bool) -> [a] -> [a]


remove p xs = filter (not . p) xs

--Q3
count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (\a -> a == x) xs

--Q4
maximum' :: Ord a => [a] -> a
maximum' xs = foldl1 (max) xs

--Q5
append :: [a] -> [a] -> [a]
append xs ys = xs ++ ys

--Assignment 3
--Q1
partialSums :: [Integer] -> [Integer]
partialSums [n] = [n]
partialSums (n:ns) = n : map (+n) (partialSums ns)

--Q2
powers :: Num a => a -> [a]
powers n = map (n^) [1..]

--Q3
factorials :: [Integer]
factorials = [foldl1 (*) [1..n] | n <- [1..]]

--Assignment 4
--Q1
approx n = iterate (\x -> (x + n / x) / 2) 1

--Q2
approxRoot n = snd . head $ dropWhile cond (zip list (tail list))
    where list = approx n
          cond (a, b) = abs(a - b) > 0.0001          

--Q3
primes = 2 : [p | p <- [3,5..], all (relativelyPrime p) (candidateDivisors p)]
    where relativelyPrime p n = gcd p n == 1
          candidateDivisors p = takeWhile (\n -> n*n <= p) primes

main = print $ take 1000 primes