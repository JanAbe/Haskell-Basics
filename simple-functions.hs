{-
Simple functions written in haskell for educational purposes and to learn / become more comfortable with recursion
Based on 'Learn you a Haskell' and 'Google\'s codelab'
-}

import Control.Monad        (void)
import Text.Read            (readMaybe)

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

-- [True, True, True, True] -> True
-- [False, True, True, True] -> False
and' :: [Bool] -> Bool
and' [] = True
and' [x]
    | x = True
    | otherwise = False
and' (x:xs)
    | x && and' xs = True
    | otherwise = False

-- [] -> True
-- [1] -> False
-- Can this be done cleaner?
-- It feels unnecessary to pattermatch for both [x] and (x:xs)
null' :: [a] -> Bool
null' [] = True
null' [x] = False
null' (x:xs) = False

-- Again, it feels unnecessary to pattermatch for both [x] and (x:xs)
head' :: [a] -> a
head' [] = error "head: empty list"
head' [x] = x
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = error "tail: empty list"
tail' [x] = []
tail' (x:xs) = xs

-- [False, True, False]
or' :: [Bool] -> Bool
or' [] = False
or' [x]
    | x = True
    | otherwise = False
or' (x:xs)
    | x || or' xs = True
    | otherwise = False

-- map (+3) [1,2,3] -> [4,5,6]
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter (>3) [1,2,3,4,5] -> [4,5]
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x  = x : filter' p xs
    | otherwise = filter' p xs

-- foldl (-) 0 [1,2,3,4]   ==   (((0 - 1) - 2) - 3) - 4   ==   -10
foldl' :: (a -> x -> a) -> a -> [x] -> a
foldl' _ a [] = a
foldl' f a [x] = f a x
foldl' f a (x:xs) = foldl' f (f a x) xs

-- foldr (-) 0 [1,2,3,4]   ==   1 - (2 - (3 - (4 - 0)))   ==    -2
-- don't know how to do this one yet :c
foldr' :: (x -> a -> a) -> a -> [x] -> a
foldr' _ a [] = a
foldr' f a [x] = f x a
-- foldr' f a (x:xs) = foldr' f (f x a) xs


-- ----------------------------------------------------------------------------

-- Rock, Paper, Scissors (from google's codelab)
-- Only the functions 'pairScore' and 'score' have been implemented by me
-- the others were already provided 

-- New Datatype with the following value constructors (Rock, paper, scissors)
data Hand = Rock | Paper | Scissors deriving (Show, Read, Eq)
-- Type synonym, different name for a type so it makes more sense
type Score = (Int, Int)

-- "winsOver" tells you if a hand wins over another one.  It introduces a
-- nifty trick: any binary function can be used in an infix way if
-- surrounded by backquotes.
winsOver :: Hand -> Hand -> Bool
Rock `winsOver` Scissors = True
Paper `winsOver` Rock = True
Scissors `winsOver` Paper = True
_ `winsOver` _ = False

computeScore :: Hand -> Hand -> Score
computeScore h1 h2
    | h1 `winsOver` h2 = (1, 0)
    | h2 `winsOver` h1 = (0, 1)
    | otherwise        = (0, 0)

combine :: Score -> Score -> Score
combine (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

-- Implemented this myself, the other functions were already provided!!
pairScore :: (Hand, Hand) -> Score
pairScore (h1, h2) = computeScore h1 h2

-- Implemented this myself, the other functions were already provided!!
-- [Paper, Scissors, Paper, Paper]
-- [Rock, Rock, Paper, Scissors]
-- [(Paper, Rock), (Scissors, Rock), (Paper, Paper), (Paper, Scissors)]
-- iterate over it, computeScore for each pair, combine each score
score :: [Hand] -> [Hand] -> Score
score h1 h2 = foldl combine (0, 0) $ map pairScore $ zip h1 h2

-- Play up to 3
gameOver :: Score -> Bool
gameOver (s1, s2) = s1 >= 3 || s2 >= 3

readHand :: String -> IO Hand
readHand prompt = do
  putStr prompt                  -- prints the prompt
  handText <- getLine            -- reads one line of input
  case readMaybe handText of     -- tries to convert it to Hand
     Just h  -> return h         -- success: our result is h
     Nothing -> readHand prompt  -- failure: we try again

playTurn :: Score -> IO Score
playTurn oldScore = do
  h1 <- readHand "p1: "
  h2 <- readHand "p2: "
  let turnScore = computeScore h1 h2
      newScore  = combine oldScore turnScore
  print newScore
  if gameOver newScore
    then return   newScore
    else playTurn newScore

play :: IO ()
play = void $ playTurn (0,0)