-- File/Module name: Dates (.hs)
-- Author: <Patrick Dong>, u<6683369>
-- Date: <May 8,2018>
-- Description: Provides types, names and functions for dates.
module Dates
  ( Date(Date, day', month', year')
  , Day
  , Month
  , Year
  , Days(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
     Sunday)
  , Months(January, February, March, April, May, June, July, August,
       September, October, November, December)
  , iso_day_no_to_name
  , day_name_to_iso_day_no
  ) where
import Integer_Subtypes

type Day = Positive -- 1..31

type Month = Months

type Year = Natural

data Date = Date
  { day' :: Day
  , month' :: Month
  , year' :: Year
  } deriving (Eq, Show)

data Days
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Enum, Show)

data Months
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Enum, Show)

-- Convert between ISO day numbers (Monday = 1, .., Sunday = 7) and day names
iso_day_no_to_name :: Positive -> Days
iso_day_no_to_name iso_day_no = toEnum (fromInteger (toInteger iso_day_no - 1))

day_name_to_iso_day_no :: Days -> Positive
day_name_to_iso_day_no day = fromInteger (1 + toInteger (fromEnum day))