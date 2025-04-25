module Crom.Calendar (
    Month,
    Date,
    Season,
    perennial
) where

import qualified Data.Set as S

data Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
    deriving (Show, Eq, Ord, Enum, Bounded)

data Date = Date {year :: Int, month :: Month, day :: Int}
    deriving Show

type Season = S.Set Month

perennial :: Season
perennial = S.fromList [minBound, maxBound]
