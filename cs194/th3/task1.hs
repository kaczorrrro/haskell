module Gofl where
import Data.Char

all_nth n list = [val | (id, val) <- (zip [1..] list), id `mod` n == 0]
skips :: [a] -> [[a]]
skips list = [all_nth n list | n <- [1..length list]]






localMaxima :: [Integer] -> [Integer]
-- localMaxima (a:b:c:tail) 
--     | a < b && b > c = b : localMaxima (c:tail)
--     | otherwise = localMaxima (b:c:tail)
-- localMaxima _ = []

localMaxima list = 
    [b | (a,b,c) <- zip3 list (drop 1 list) (drop 2 list), a < b, b > c]

-- localMaxima list = 
--     [b | (a,b,c) <- (zip3 (d 0) (d 1) (d 2)), a < b, b > c] where d n = drop n list 

text = "[b | (a,b,c) <- zip3 list (drop 1 list) (drop 2 list), a < b, b > c]"
text_len = length (filter (not . isSpace) text)    


-- type Counter = [Int]

-- ctr_inc :: Counter -> Int -> Counter
-- ctr_inc c pos = (take pos c) ++ [(c !! pos) + 1] ++ (drop (pos + 1) c)

-- ctr_init = replicate 10 0 :: Counter

-- counts :: [Integer] -> Counter
-- counts list = foldl ctr_inc ctr_init (map fromIntegral list)

-- level_str :: Counter -> Int -> String
-- level_str ctr level = [if val >= level then '*' else ' ' | val <- ctr]

-- legend = ["==========", "0123456789"]

-- ctr_str ctr = unlines $
--     [ level_str ctr level | level <- reverse [1..(maximum ctr)] ] ++ legend

-- histogram :: [Integer] -> String
-- histogram list = ctr_str $ counts list


-- Shorter impl (no explicit counter)
count :: [Integer] -> Integer -> Int
count list val = length $ filter (val==) list

level_str :: [Integer] -> Int -> String
level_str list level = [if count list val >= level then '*' else ' ' | val <- [0..9]]

levels :: [Integer] -> [Int]
levels list = reverse [1..m] where m = maximum $ map (count list) [0..9]

histogram :: [Integer] -> String
histogram list = unlines $
    [ level_str list level | level <- levels list ] ++ ["==========", "0123456789"]

