-- How to make it work on anything?
myReverse :: [Integer] -> [Integer]
myReverse [] = []
myReverse (head:tail) = myReverse tail ++ [head]

charToInt :: Char -> Integer
charToInt c = read [c]

toDigits :: Integer -> [Integer]
toDigits x 
    | x > 0     = map charToInt (show x)
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)
 
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = [x, y * 2] ++ doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c  = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a 

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 a b c d = []
hanoi4 1 a b c d = [(a, b)]
hanoi4 2 a b c d = [(a, c), (a, b), (c, a)]
hanoi4 3 a b c d = [(a, c), (a, d), (a, b), (d, b), (c, d)]
hanoi4 4 a b c d = hanoi 2 a c d ++ [(a, b)] ++ hanoi 2 c b d
hanoi4 n a b c d = 
    hanoi4 (n-4) a c b d ++ hanoi 4 a b d ++ hanoi4 (n-4) c b a d 
