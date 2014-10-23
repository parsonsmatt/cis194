-- Exercise 1:

toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0    = []
	| n <  10   = [n]
	| otherwise = concat [toDigits(div n 10), [mod n 10]] 

-- toDigitsRev :: Integer -> [Integer]


