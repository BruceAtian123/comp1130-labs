module Polymorphic where

import Data.Complex
import Data.Ratio
import Prelude hiding (product)

stringLength :: String -> Int
stringLength string = case string of
        [] -> 0
        x:xs -> 1 + stringLength xs

integerListLength :: [Integer] -> Int
integerListLength list = case list of
        [] -> 0
        y:ys -> 1 + integerListLength ys

-- | A polymorphic length function
polymorphicLength :: [a] -> Int
polymorphicLength list = case list of
  []     -> 0
  _ : xs -> 1 + polymorphicLength xs

-- | A polymorphic reverse function
reverseOf :: [a] -> [a]
reverseOf list = case list of
  []    -> []
  x:xs -> reverse (x:xs) []
    where reverse :: [a] -> [a] -> [a]
          reverse (x:xs) ys = reverse xs (x:ys)
          reverse [] x = x

-- | A polymorphic isPalindrome function
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list
  | list == reverseOf list = True
  | otherwise = False

-- | A polymorphic list equality function
listEqual :: (Eq a) => [a] -> [a] -> Bool
listEqual list1 list2 = case (list1, list2) of
    ([], [])    -> True
    ([], _)     -> False
    (_,[])      -> False
    (x:xs, y:ys) -> (x == y) && (xs `listEqual` ys)

isMonotonicallyIncreasing :: [Integer] -> Bool
isMonotonicallyIncreasing list = case list of
    [] -> True
    [_] -> True
    x:y:xs | x<=y -> isMonotonicallyIncreasing (y:xs)
           | otherwise -> False

normaliseVector :: [Float] -> [Float]
normaliseVector vector = divideByScalar vector (norm vector)

divideByScalar :: [Float] -> Float -> [Float]
divideByScalar vector' scalar = case vector' of
         []    -> []
         f: fs -> (f / scalar): (divideByScalar fs scalar)

norm :: [Float] -> Float
norm vector' = sqrt (sumSqr vector')
      where sumSqr :: [Float] -> Float
            sumSqr vector'' = case vector'' of
               []    -> 0
               f: fs -> f*f + (sumSqr fs)

reallyPolymorphicLength :: (Integral b) => [a] -> b
reallyPolymorphicLength list = case list of
  []    -> 0
  _:xs -> 1 + reallyPolymorphicLength xs

printLargestAsString :: (Ord a, Show a) => [a] -> String
printLargestAsString list =
  "The largest element in the list is " ++ show (maximum list)

rationalZero :: Ratio Integer
floatZero    :: Float
doubleZero   :: Double
complexZero  :: Complex Double

rationalZero = 0
floatZero    = 0.0
doubleZero   = 0.0
complexZero  = mkPolar 0.0 0.0

multiply :: (Num b) => [b] -> b
multiply list = case list of
  []    -> undefined
  x:xs  -> x*(multiply xs)

product' :: (Num a, Show a) => [a] -> String
product' list = "The product of " ++ show (list) ++ " is " ++ show (multiply list)