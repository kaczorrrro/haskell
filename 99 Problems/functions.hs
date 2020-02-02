-- 1
myLast :: [x] -> x
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:x) = myLast x

-- 2
myButLast :: [x] -> x
myButLast []  = error "Empty list"
myButLast [_] = error "One elem list"
myButLast [x,y] = x
myButLast (_:x) = myButLast x 

-- 3, but 0 based
elementAt :: (Integral i) => [x] -> i -> x
elementAt (x:_) 0 = x
elementAt (_:x) n = elementAt x (n-1) 

-- -1 based hehe
weirdElementAt :: (Integral i) => [x] -> i -> x
weirdElementAt (x:_) (-1) = x
weirdElementAt (_:x) n  = weirdElementAt x (n-1)

-- 4
myLen :: [x] -> Int
myLen [] = 0
myLen (_:x) = myLen x + 1

-- 5
myReverse :: [x] -> [x]
myReverse [] = []
myReverse (x:y) = myReverse y ++ [x]

myReverse2 :: [x] -> [x]
myReverse2 list  = myReverse2' list []
    where
        myReverse2' [] reversed = reversed
        myReverse2' (x:xs) reversed = myReverse2' xs (x:reversed) 

-- 6
palindrome :: (Eq x) => [x] -> Bool
palindrome x = x == myReverse x

-- 7 TODO
data NestedList a = Elem a | List [NestedList a]

-- 8
compress :: (Eq x) => [x] -> [x]
compress list = reverse (compress' list [])
    where 
        compress' [] y  = y
        compress' (x:xs) [] = compress' xs [x]
        compress' (x:xs) out@(y:_) = compress' xs (if x /= y then x:out else out)

compress2 :: (Eq x) => [x] -> [x]
compress2 (x:xs@(y:_))
    | x == y = compress2 xs
    | x /= y = x : compress2 xs
compress2 x = x

-- 9
pack :: (Eq x) => [x] -> [[x]]
pack (x:xs@(y:_))
    | x == y = let (lastGrp:grps) = pack xs in (x : lastGrp) : grps  
    | x /= y = [x] : pack xs
pack list = [list]

-- 10
encode :: (Eq x) => [x] -> [(Int, x)]
encode list = encode' (pack list)
    where 
        encode' (x:xs) = (length x, head x) : encode'(xs)
        encode' [] = []

--List copy vs reverse
copy' :: [x] -> [x]
copy' [] = []
copy' (x:xs) = x : copy'(xs)

reverse' :: [x] -> [x]
reverse' x = reverse'' x []
    where
        reverse'' :: [x] -> [x] -> [x]
        reverse'' [] reversed = reversed
        reverse'' (x:xs) reversed = reverse'' xs (x:reversed)

-- 11
encodeModified :: (Eq x) => [x] -> [(String, x)]
encodeModified list = encodeModified' (pack list)
    where 
        encodeModified' (x:xs) = 
            let len = length x
                newHead = if len == 1 
                    then ("Single", head x) 
                    else ("Multi ", head x)
            in newHead : encodeModified'(xs)
        encodeModified' [] = []