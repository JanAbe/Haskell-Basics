{-
Simple functions written in haskell for educational purposes and to learn / become more comfortable with recursion
-}


-- [35,6,1,13,20,40]
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = sortedSmallerNumbers ++ [x] ++ sortedBiggerNumbers
                   where sortedSmallerNumbers = quicksort [a | a <- xs, a <= x]
                         sortedBiggerNumbers = quicksort [a | a <- xs, a > x]

-- 4! -> 4*3*2*1 = 24
-- 4 * 3!
-- 4 * 3 * 2!
-- 4 * 3 * 2 * 1!
factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial x = x * factorial (x-1)

-- take' 3 [1,2,3,4,5] -> [1,2,3]
-- take' 1 [1,2,3,4,5] -> [1]
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

-- maximum' [1,2,3,4,5]
maximum' :: (Num i, Ord i) => [i] -> i
maximum' [] = error "not possible"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- product' [1,2,3,4] = 1 * product [2,3,4]
product' :: (Integral i) => [i] -> i
product' [] = error "not possible"
product' [x] = x
product' (x:xs) = x * product' xs

-- sum [1,4,5,1] -> 11
-- = 1 + sum [4,5,1]
sum' :: (Num n) => [n] -> n
sum' [] = error "no!"
sum' [x] = x
sum' (x:xs) = x + sum' xs

-- replicate' 3 5 -> [5,5,5]
replicate' :: (Integral i, Ord i) => i -> i -> [i]
replicate' n _
    | n <= 0 = []
replicate' n x = x:replicate' (n-1) x

-- [1,2,3,4,5] `contains` 5 -> True
-- contains [1,2,3] 5
contains' :: (Integral i, Eq i) => [i] -> i -> Bool
contains' [] _ = False
contains' (x:xs) a
    | x == a = True
    | otherwise = contains' xs a

-- zip [1,2,3] [4,5,6] -> [(1,4), (2,5), (3,6)]
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- [1,2,3,4], 1 + length' xs
length' :: [a] -> Int
length' [] = 0
length' [x] = 1
length' (x:xs) = 1 + length' xs

-- [True, True, True, True]
and' :: [Bool] -> Bool
and' [] = error "Not possible :("
and' [x]
    | x == True = True
    | otherwise = False
and' (x:xs)
    | x == True && and' xs == True = True
    | otherwise = False