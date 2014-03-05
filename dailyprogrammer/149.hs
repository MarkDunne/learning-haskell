import Data.List

main = do
	str <- getLine
	let (a, b) = partition (`elem` "aeiou") $ filter (/= ' ') str
	putStr $ unlines [a, b]