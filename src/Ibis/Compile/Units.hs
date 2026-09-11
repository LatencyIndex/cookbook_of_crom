module Ibis.Compile.Units (
    Unit (..),
    toUnit,
) where

import qualified Crom.Units as U
import qualified Data.Text as T

data Unit
    = Mass U.Mass
    | Energy U.Energy
    | SpecificGravity
    | InternationalUnit

-- | Ignores whitespace
toUnit :: T.Text -> Either T.Text Unit
toUnit s = case T.strip s of
    "IU" -> Right InternationalUnit
    "g" -> Right $ Mass U.g
    "kJ" -> Right $ Energy U.kJ
    "kcal" -> Right $ Energy U.kcal
    "mg" -> Right $ Mass U.mg
    "mg_ATE" -> Right $ Mass U.mg
    "sp gr" -> Right SpecificGravity
    "µg" -> Right $ Mass U.ug
    x -> Left x
