module Crom.Calendar where

type Year = Int
type Month = Int -- Numbered 1 - 12
type Day = Int
data Date = Date {year :: Year, month :: Month, day :: Day} deriving (Show, Eq, Ord)

-- Closed interval of months that wraps around.
-- If start <= end, the season contains the months [start, end]
-- Otherwise, it contains the months [start, Dec] and [Jan, end]
data Season = Season {start :: Month, end :: Month} deriving Show

in_season :: Month -> Season -> Bool
in_season x (Season a b)
    | a <= b = a <= x && x <= b
    | otherwise = x <= a || b <= x
