doubleMe :: Int -> Int
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmall x = (if x > 100 then x else doubleMe x) + 1

filter_10 xs = [if x > 10 then ">10" else  "<10" | x <- xs, odd x]
len' x = sum[1 | _ <- x]

nestedListDouble xxs = [[x*2 | x<- xs] | xs <- xxs]

letterConvTable = zip ['a'..'z'] ['A'..'Z']
toLower str = [ if x `elem` ['A'..'Z'] then fst (head [tpl | tpl <-letterConvTable, snd tpl == x]) else x | x <- str]
toLower2 str = [ 
    if x `elem` [snd tpl | tpl <- letterConvTable] 
        then fst (head [tpl | tpl <-letterConvTable, snd tpl == x]) 
        else x | x <- str]

rightTriangles = [(a,b,c) | a <- [1..100], b <- [1..100], c <- [1..100], a^2 + b^2 == c^2]


lucky :: Int -> String
lucky 7 = "Lucky"
lucky x = "Not lucky"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = factorial (n - 1) * n

addVec :: (Integral a) => (a, a) -> (a, a) -> (a, a)
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Head on empty list"
head' (x:_) = x

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = if n `mod` 2 == 0 then n : collatz (n `div` 2) else n : collatz (n*3+1)

numLongChains :: Integer -> Int -> Int
numLongChains maxVal minLen = length (filter (isLong minLen) (map collatz [1..maxVal]))
    where
        isLong minLen x = length x >= minLen

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show
-- area :: Shape -> Float
-- area (Circle _ _ r) = pi * r ^ 2
-- area (Rectangle x1 y1 x2 y2) = abs $ (x1 - x2) * (y1-y2)

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs $ (x1 - x2) * (y1-y2)