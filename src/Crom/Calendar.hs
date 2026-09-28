module Crom.Calendar (
    Month (..),
    Date,
    perennial,
) where

import Data.Set qualified as S

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
    deriving (Show)

perennial :: S.Set Month
perennial = S.fromList [minBound .. maxBound]
