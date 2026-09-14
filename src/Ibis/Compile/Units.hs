module Ibis.Compile.Units (
    Unit (..),
    toUnit,
    toMass,
    toEnergy,
) where

import qualified Crom.Units as U
import qualified Data.Text as T

-- | Units used in the JSON food databases
data Unit
    = Mass U.Mass
    | Energy U.Energy
    | SpecificGravity
    | InternationalUnit
    | Unknown T.Text

-- | Ignores whitespace
toUnit :: T.Text -> Unit
toUnit s = case T.strip s of
    "IU" -> InternationalUnit
    "g" -> Mass U.g
    "kJ" -> Energy U.kJ
    "kcal" -> Energy U.kcal
    "mg" -> Mass U.mg
    "mg_ATE" -> Mass U.mg -- Milligrams of Alpha-Tocopherol Equivalents (Vitamin E)
    "sp gr" -> SpecificGravity -- Density relative to water: https://en.wikipedia.org/wiki/Relative_density
    "µg" -> Mass U.ug
    x -> Unknown x

toMass :: Unit -> Maybe U.Mass
toMass (Mass x) = Just x
toMass _ = Nothing

toEnergy :: Unit -> Maybe U.Energy
toEnergy (Energy x) = Just x
toEnergy _ = Nothing
