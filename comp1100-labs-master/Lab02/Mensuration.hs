-- COMP1100
-- Semester 1, 2018
-- Week 2 Lab
-- <Patrick Dong>, February 2018
module Mensuration where
-- Here are a few simple mensuration definitions:
cube :: Integer -> Integer
cube x = x * x * x

edgeLength :: Integer
edgeLength = 3

volume :: Integer
volume = cube edgeLength

-- If I remember correctly, this is the formula for the surface area
-- of a sphere, in terms of its radius:
surfaceAreaWithRadius :: Float -> Float
surfaceAreaWithRadius r = 4.0 * pi * r^2

isValidTriangle :: Float -> Float -> Float -> Bool
isValidTriangle a b c
  | a + b > c && b + c > a && a + c > b = True
  | otherwise = False

areaOfTriangle :: Float -> Float -> Float -> Maybe Float
areaOfTriangle a b c = Just (sqrt(s*(s-a)*(s-b)*(s-c)))
   where
      s = (a + b + c)/2

data Quadrants = Origin |
                 QuadrantI | QuadrantII | QuadrantIII | QuadrantIV |
                 XAxisPositive | XAxisNegative | YAxisPositive | YAxisNegative
  deriving (Show, Eq)
quadrant :: Float -> Float -> Quadrants
quadrant x y
  | x > 0 = atan(y/x)
  | x < 0 && y >= 0 = atan(y/x) + pi
  | x < 0 && y < 0 = atan(y/x) - pi
  | x = 0 && y > 0 = (pi/2)
  | x = 0 && y < 0 = -(pi/2)
  | x = 0 && y = 0 = undefined