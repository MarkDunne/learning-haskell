import Data.List

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
        where (before, after) = split xs (n-1)

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


main = print $ range 4 7