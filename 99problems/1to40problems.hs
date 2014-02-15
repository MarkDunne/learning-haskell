import Debug.Trace
import Data.List
import System.Random
import Control.Monad
import Data.Function

--Problem 3
elementAt :: Eq a => [a] -> Integer -> a
elementAt [] n = error "index out of bounds"
elementAt (x:xs) n
    | n > 1 = elementAt xs (n-1)
    | otherwise = x

--Problem 4
myLength :: [a] -> Integer
myLength xs = foldr (\_ -> (+1)) 0 xs

--Problem 5
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

--Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = and $ map (\(a, b) -> a == b) (zip xs (reverse xs))

--Problem 7
flatten :: [[a]] -> [a]
flatten xs = foldl1 (++) xs

--Problem 8
--Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress xs = map head (group xs)

myCompress :: Eq a => [a] -> [a]
myCompress [] = []
myCompress (x:xs) = x : (myCompress rest)
    where rest = dropWhile (==x) xs

--Problem 9
--Pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : same) : pack rest
    where (same, rest) = span (==x) xs

--Problem 10
--Find length of runs from Problem 9
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- group xs]

myEncode :: Eq a => [a] -> [(Int, a)]
myEncode input =
    let packed = pack input
        lengths = map length packed
        letters = map head packed
    in zip lengths letters

--Problem 11
data Encoded a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified input =
    let encoded = encode input
        modifier (1, a) = Single a
        modifier (n, a) = Multiple n a
    in map modifier encoded

--Problem 12
decodeModified :: [Encoded a] -> [a]
decodeModified input = concatMap decoder input
    where decoder (Single a) = [a]
          decoder (Multiple n a) = replicate n a

--Problem 14
duplicate' xs = xs >>= (\x -> [x,x]) --using monads

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

--Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

--Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs 1
    where helper [] counter = [] 
          helper (x:xs) counter
            | counter `mod` n == 0 = rest
            | otherwise = x : rest
                where rest = helper xs (counter+1)


--Problem 17
split :: [a] -> Int -> ([a], [a])
split (x:xs) n
    | n > 1 = (x: before, after)
    | otherwise = ([x], xs)
        where (before, after) = Main.split xs (n-1)

--Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs lo hi = take (hi-lo+1) $ drop (lo -1 ) xs

mySlice :: [a] -> Int -> Int -> [a]
mySlice (x:xs) start end
    | end < 1 = []
    | start > 1 = mySlice xs (start-1) (end-1)
    | start <= 1 = x : mySlice xs (start-1) (end-1)

--Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = front ++ back
    where r = n `mod` length xs
          (back, front) = splitAt r xs

--Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (element, before ++ after)
    where (beforeInclusive, after) = splitAt n xs
          element = last beforeInclusive
          before = init beforeInclusive


--Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt new xs n = before ++ [new] ++ after
    where (before, after) = splitAt (n - 1) xs

--Problem 22
range :: Enum a => a -> a -> [a]
range start end = [start..end]

--Problem 23
rndElem :: [a] -> IO a
rndElem xs = do
    n <- randomRIO(0, length xs - 1)
    return $ xs !! n

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = replicateM n (rndElem xs)

--Problem 24
rndDistinctElems :: [a] -> Int -> IO [a]
rndDistinctElems __ 0 = return []
rndDistinctElems xs n = do
    gen <- getStdGen
    rand <- randomRIO(1, length xs)
    let (elem, remaining) = removeAt rand xs
    rest <- rndDistinctElems remaining (n-1)
    return $ elem : rest

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndDistinctElems [1..m] n

--Problem 25
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rndDistinctElems xs (length xs)

--Problem 26
combinations :: [a] -> Int -> [[a]]
combinations __ 0      = [[]]
combinations [] _     = []
combinations (x:xs) m = map (x:) (combinations xs (m-1)) ++ combinations xs m

--Problem 27

--Problem 28
lsort :: [[a]] -> [[a]]
lsort xs = sortBy f xs
    where f a b = compare (length a) (length b) 

fsort :: [[a]] -> [[a]]
fsort xs = concat . lsort $ groupBy f (lsort xs)
    where f a b = (length a) == (length b)

--Problem 31
primes :: [Int]
primes = 2 : 3 : [x+i | x <- [6,12..], i <- [-1, 1], all (relativelyPrime $ x+i) (candidateDivisors $ x+i)]
    where relativelyPrime p n = not $ p `mod` n == 0
          candidateDivisors p = takeWhile (\n -> n*n <= p) primes

isPrime :: Int -> Bool
isPrime n = any ((/=0) . mod n ) (takeWhile (\p -> p*p<=n) primes)

--Problem 32
myGcd :: Int -> Int -> Int 
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b)

--Problem 33
coprime :: Int -> Int -> Bool
coprime a b = (myGcd a b) == 1

--Problem 34
totient :: Int -> Int
totient x = length [n | n <- [1..x], coprime x n]

--Problem 35
primeFactors :: Int -> [Int]
primeFactors n = go primes n
    where go primes@(p:ps) n
            | n `mod` p == 0 = p : go primes (n `div` p)
            | p > n = []
            | otherwise = go ps n

--Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = encode $ primeFactors n

--Problem 37
phi :: Int -> Int
phi n = product [(p - 1) * p ^ (m - 1) | (m, p) <- primeFactorsMult n]

--Problem 39
primeR :: Int -> Int -> [Int]
primeR lo hi = takeWhile (\p -> p < hi) $ dropWhile (\p -> p < lo) primes

--Problem 40
goldbach :: Int -> (Int, Int)
goldbach n = head [(p1, p2) | p1 <- primeR 2 n, p2 <- primeR p1 (n-p1+1), p1 + p2 == n]

--Problem 41 Part 1
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList lo hi = map goldbach (filter even [lo..hi])

--Problem 41 Part 2
goldbachList' :: Int -> Int -> Int -> Int
goldbachList' lo hi n = sum [1 | (a, b) <- goldbachList lo hi, a >= n || b >= n]

main = print $ goldbachList' 4 3000 50