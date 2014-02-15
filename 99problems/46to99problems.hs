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

--Problem 47
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ print output
    where output = [inputs ++ [f inputs] | inputs <- replicateM n [True, False]]

--Problem 48
binaryToGray :: [Int] -> [Int]
binaryToGray num = xor shifted num
    where shifted = 0 : init num
          xor = zipWith (\a b -> mod (a + b) 2)

gray :: Int -> [[Int]]
gray n = map binaryToGray (replicateM n [0, 1])

main = print $ gray 3