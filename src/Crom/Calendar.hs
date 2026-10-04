{-# LANGUAGE DeriveAnyClass #-}

module Crom.Calendar (
    Month (..),
    Date,
    perennial,
) where

import Data.Binary (Binary)
import Data.Set qualified as S
import GHC.Generics (Generic)

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
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Binary)

data Date = Date {year :: Int, month :: Month, day :: Int}
    deriving (Show)

perennial :: S.Set Month
perennial = S.fromList [minBound .. maxBound]
