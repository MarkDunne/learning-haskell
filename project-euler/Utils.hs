module Utils where
	numDigits :: Integer -> Integer
	numDigits n = floor (logBase 10 (fromInteger n)) + 1