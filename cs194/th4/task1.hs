-- Ex 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x -> x - 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum .
    filter even .
    takeWhile (>1) .
    iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Ex 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)


height :: Tree a -> Integer
height Leaf = -1;
height (Node h _ _ _) = h;


addToTree :: a -> Tree a -> Tree a
addToTree a Leaf = Node 0 Leaf a Leaf

addToTree a (Node h left val right)
    | height left <= height right =
        let
            newtree = addToTree a left
            newheight = max h (height newtree + 1)
        in  Node newheight newtree val right
    | otherwise                  =
        let
            newtree = addToTree a right
            newheight = max h (height newtree + 1)
        in Node newheight left val newtree

foldTree :: [a] -> Tree a
foldTree list = foldr addToTree Leaf list



xor' :: Bool -> Bool -> Bool
xor' x y = (x || y) && not (x && y)

xor :: [Bool] -> Bool
xor = foldr (xor') False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []


fst' :: a -> a -> a
fst' a _ = a

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
-- Tested on ** operator



-- cartProd :: [a] -> [b] -> [(a, b)]
-- cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- formula :: (Int, Int) -> Int
-- formula (i, j) = i + j + 2 * i * j

-- isGoodPair:: Int -> (Int, Int) -> Bool
-- isGoodPair n pair@(i, j) = i <= j && (formula pair) <= n

-- getExcludedPairs:: Int -> [(Int, Int)]
-- getExcludedPairs n = filter (isGoodPair n) (cartProd [1..n] [1..n])


-- sieveSundaram :: Int -> [Int]
-- sieveSundaram n = map (\x -> x * 2 + 1) $
--     filter (\a -> not $ any (\pair -> (formula pair) == a) (getExcludedPairs n)) [1..n]

sundDel :: Integer -> [Integer]
sundDel n = filter (<n) [i + j + 2 * i * j | i <- [1..n], j <-[i..n]]

sund :: Integer -> [Integer]
sund n = map (\x -> x * 2 + 1) $ filter (\x -> not (x `elem` (sundDel n))) [1..n]