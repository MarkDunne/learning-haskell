import Data.Ord
import Data.List
import Control.Monad

--Problem 46 Part 1
and', or', nand', nor', impl', equ' :: Bool -> Bool -> Bool

or'  = (||)
and' = (&&)
equ' = (==)
nand' a b = not (and' a b)
nor'  a b = not (or' a b)
xor'  a b = not (equ' a b)
impl' a b = or' (not a) b

--Problem 46 Part 2
table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ print output
    where output = [[a, b, (f a b)] | a <- [True, False], b <- [True, False]]

--Problem 48
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ print output
    where output = [inputs ++ [f inputs] | inputs <- replicateM n [True, False]]

--Problem 49
binaryToGray :: [Int] -> [Int]
binaryToGray num = xor shifted num
    where shifted = 0 : init num
          xor = zipWith (\a b -> mod (a + b) 2)

gray :: Int -> [[Int]]
gray n = map binaryToGray (replicateM n [0, 1])

--Problem 50
--huffman :: [(Char, Int)] -> [(Char, String)]

data HTree a = Leaf a | Branch (HTree a) (HTree a)
	deriving (Show)

hstep :: [(Int, HTree Char)] -> [(Int, HTree Char)]
hstep ls = combineSmallest $ sortBy (comparing fst) ls
	where combineSmallest ((w1,l1):(w2,l2):ls) = [(w1 + w2, Branch l1 l2)] ++ ls

serialize :: HTree Char -> [(Char, String)]
serialize (Branch left right) = 
	[(x, '0':prefix) | (x, prefix) <- serialize left] ++
	[(x, '1':prefix) | (x, prefix) <- serialize right]
serialize (Leaf x) = [(x, "")]

huffEncode :: [(Char, Int)] -> [(Char, String)]
huffEncode input = serialize . snd . head $ until (\ls -> length ls == 1) hstep (setupData input)
	where setupData freqs = [ (w, Leaf e) | (e, w) <- sortBy (comparing snd) freqs]

main = print $ "hello"