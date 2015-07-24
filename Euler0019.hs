module Euler0019 where

import Common.Math
import Common.List

type Year = Integer
type Day  = Integer

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Enum, Eq)

data WDay = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Enum, Eq)

data Date = Date Year Month Day
  deriving (Eq)

isLeapYear :: Year -> Bool
isLeapYear y = 4 `divides` y && (100 `divides` y) `implies` (400 `divides` y)

isLastOfMonth :: Date -> Bool
isLastOfMonth (Date y m d) =
  case m of
   Jan -> d == 31
   Feb -> if isLeapYear y
          then d == 29
          else d == 28
   Mar -> d == 31
   Apr -> d == 30
   May -> d == 31
   Jun -> d == 30
   Jul -> d == 31
   Aug -> d == 31
   Sep -> d == 30
   Oct -> d == 31
   Nov -> d == 30
   Dec -> d == 31

nextMonth :: Month -> Month
nextMonth = toEnum . (`mod` 12) . (+1) . fromEnum

nextWDay :: WDay -> WDay
nextWDay = toEnum . (`mod` 7) . (+1) . fromEnum

tomorrow :: Date -> Date
tomorrow (Date y m d)
  | isLastOfMonth (Date y m d) = Date (if m == Dec then y + 1 else y) (nextMonth m) 1
  | otherwise = Date y m (d + 1)

dateRange = iterate tomorrow (Date 1900 Jan 1) `upTo'` (Date 2000 Dec 31)
wdayRange = iterate nextWDay Mon

dateAndWeek = zipWith (,) dateRange wdayRange

solution = count (\(Date _ _ d, wd) -> d == 1 && wd == Sun)
         $ dateAndWeek `uponSatisfying` ((== Date 1901 Jan 1) . fst)
