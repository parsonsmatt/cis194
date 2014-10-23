-- Exercise 1:

toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0    = []
	| n <= 9    = [n]
	| otherwise = concat [toDigits(div n 10), [mod n 10]] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverseList (toDigits n)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = concat [reverseList(xs), [x]]

-- Exercise 2:

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [n] = [n]
doubleEveryOther (n:m:xs) = n:m*2:doubleEveryOther(xs)

-- Exercise 3:

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [n] 
	| n <= 9    = n
	| otherwise = sumDigits(toDigits n)  
sumDigits (x:xs) = sumDigits(toDigits x) + sumDigits xs

-- Exercise 4:

validate :: Integer -> Bool
validate n =
	sumDigits (doubleEveryOther (toDigits (n))) `mod` 10 == 0	

-- Towers of Hanoi!

-- Exercise 5:

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
