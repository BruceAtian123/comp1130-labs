module HigherOrder where

import Prelude hiding (sum,product)
import Data.List
import Data.Char

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

-- | isEqual
-- Examples:
--
-- >>> isEqual 5 5.9999
-- False
--
-- >>> isEqual 6 5.9999
-- True
isEqual :: Integer -> Double -> Bool
isEqual m x = abs (fromIntegral m - x) < tolerance
  where
    tolerance = 0.0001

-- | flipArguments
-- Examples:
--
-- >>> flipArguments isEqual 5.9999 6
-- True
flipArguments :: (Integer -> Double -> Bool) -> Double -> Integer -> Bool
flipArguments func doubleValue integerValue = func integerValue doubleValue

-- | applyFunction
applyFunction :: (Integer -> Integer) -> Integer -> Integer
applyFunction f x = f(x)

double :: Integer -> Integer
double m = 2 * m

triple :: Integer -> Integer
triple m = m + m + m

reverseSign :: Integer -> Integer
reverseSign m = -m

-- | applyFunctionOverList
-- Examples:
--
-- >>> applyFunctionOverList double [1,2,3]
-- [2,4,6]
--
-- >>> applyFunctionOverList reverseSign [1,2,3]
-- [-1,-2,-3]

applyFunctionOverList :: (a->a) -> [a] -> [a]
applyFunctionOverList _ [] = []
applyFunctionOverList f (x:xs) = f x : applyFunctionOverList f xs

-- | selectWhereTrue
-- Examples:
--
--  >>> selectWhereTrue isNegative [0.0, 1.0, -1.0, -9.2, 3.0]
-- [-1.0, -9.2]
--
-- >>> selectWhereTrue isPositive [0.0, 1.0, -1.0, -9.2, 3.0]
-- [1.0, 3.0]
selectWhereTrue :: (Double -> Bool) -> [Double] -> [Double]
selectWhereTrue _ [] = []
selectWhereTrue isNegative (x:xs)
    | isNegative x = [x]
    | otherwise = selectWhereTrue isNegative xs
selectWhereTrue isPositive (x:xs)
    | isPositive x = [x]
    | otherwise = selectWhereTrue isPositive xs

isNegative :: Double -> Bool
isNegative x = x < 0.0

isPositive :: Double -> Bool
isPositive x = x > 0.0

-- | generalLength
generalLength :: Integral b => [a] -> b
generalLength list =
  case list of
    [] -> 0
    _:xs -> 1 + generalLength xs

-- | applyFunction'
applyFunction' :: (a -> Integer) -> a -> Integer
applyFunction' f m = f m

-- | applyFunction''
applyFunction'' :: (a -> b) -> a -> b
applyFunction'' f x = f x

-- | applyFunctionOverList'
applyFunctionOverList' :: (a->a) -> [a] -> [a]
applyFunctionOverList' _ [] = []
applyFunctionOverList' f (x:xs) = f x : applyFunctionOverList f xs

-- | selectWhereTrue'
selectWhereTrue' :: (Double -> Bool) -> [Double] -> [Double]
selectWhereTrue' _ [] = []
selectWhereTrue' isNegative (x:xs)
    | isNegative x = [x]
    | otherwise = selectWhereTrue' isNegative xs
selectWhereTrue' isPositive (x:xs)
    | isPositive x = [x]
    | otherwise = selectWhereTrue' isPositive xs

-- | combineListsWithBinaryOperation
-- Examples:
--
-- >>> combineListsWithBinaryOperation (+) [1.0, 2.0, 3.0] [4.0, 5.0, 6.0]
-- [5.0, 7.0, 9.0]
--
-- >>> combineListsWithBinaryOperation (++) ["the", "brown", "jumps", "the"] ["quick", "fox", "over", "lazy", "dog"]
-- ["thequick", "brownfox", "jumpsover", "thelazy"]
--
-- >>> combineListsWithBinaryOperation div [1..10] [-10..0]
-- [-1, -1, -1, -1, -1, -2, -2, -3, -5, -10]
combineListsWithBinaryOperation operation list1@(x:xs) list2@(y:ys) = (x `operation` y) : combineListsWithBinaryOperation operation xs ys
combineListsWithBinaryOperation operation [] [] = []

-- | combineElementsIntoTuples
combineElementsIntoTuples :: [a] -> [b] -> [(a, b)]
combineElementsIntoTuples [] [] = []
combineElementsIntoTuples _ [] = error"No elements in one list"
combineElementsIntoTuples [] _ = error"No elements in one list"
combineElementsIntoTuples [a] [b] = [(a,b)]
combineElementsIntoTuples (x:xs) (y:ys) = [(x,y)] ++ combineElementsIntoTuples xs ys

-- | combineElementsIntoTuples'
combineElementsIntoTuples' :: a -> b -> (a,b)
combineElementsIntoTuples' a b = head (combineListsWithBinaryOperation (,) [a] [b])

-- | foldRight
-- Examples:
--
-- >>> foldRight (+) 0 [1,2,3,4,5]
-- 15
--
-- >>> foldRight (*) 1 [1,2,3,4,5]
-- 120
--
-- >>> foldRight (-) 0 [1,2,3,4,5]
-- 3
foldRight :: (a -> b -> b) -> b -> [a] -> b -- predefined as foldr
foldRight operation identity list =
  case list of
    [] -> identity
    x:xs -> x `operation` foldRight operation identity xs

-- | foldLeft
-- Examples:
--
-- >>> foldLeft (+) 0 [1,2,3,4,5]
-- 15
--
-- >>> foldLeft (-) 0 [1,2,3,4,5]
-- -15
--
-- prop> foldLeft (+) 0 xs == foldRight (+) 0 xs
--
-- The following property does not hold:
-- prop> foldLeft (-) 0 xs == foldRight (-) 0 xs
foldLeft :: (b -> a -> b) -> b -> [a] -> b -- predefined as foldl
foldLeft operation identity list =
  case list of
    [] -> identity
    x:xs -> foldLeft operation (identity `operation` x) xs

-- | Reimplement sum using foldLeft or foldRight
sum :: Num a => [a] -> a
sum list = foldLeft (+) 0 list

-- | Reimplement product using foldLeft or foldRight
product :: Num a => [a] -> a
product list = foldLeft (*) 1 list

-- | Reimplement allTrue using foldLeft or foldRight
allTrue :: [Bool] -> Bool
allTrue list = foldLeft (&&) True list

-- | Reimplement anyTrue using foldLeft or foldRight
anyTrue :: [Bool] -> Bool
anyTrue list = foldLeft (||) False list

-- | convertToLower
convertToLower :: String -> String
convertToLower list = map toLower list

-- | removeNonAlphanum
removeNonAlphanum :: String -> String
removeNonAlphanum list = filter isAlphaNum list

-- | dotProduct
dotProduct :: (Num a) => [a] -> [a] -> [a]
dotProduct list1 list2 = combineListsWithBinaryOperation (*) list1 list2

-- | isSquare
-- Examples:
--
-- >>> isSquare (10000 :: Int)
-- True
--
-- >>> isSquare (10000 :: Integer)
-- True
isSquare :: Integral a => a -> Bool
isSquare i = floor (sqrt (fromIntegral i) :: Float) ^ (2 :: Int) == i

-- | sumOfSquaresUpTo
sumOfSquaresUpTo = foldLeft (+) 0 (filter (isSquare) [1 .. 10000])

-- | isPrime
-- Examples:
--
-- >>> isPrime 2
-- True
--
-- >>> isPrime 3
-- True
--
-- >>> isPrime 4
-- False
--
-- >>> isPrime 5
-- True
--
-- >>> isPrime 6
-- False
--
-- >>> isPrime 7
-- True
isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]

-- | primeFactors
primeFactors :: Integer -> [Integer]
primeFactors n = case factors of
                [] -> [n]
                _  -> factors ++ primeFactors (n `div` (head factors))
                where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
