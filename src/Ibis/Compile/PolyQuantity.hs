module Ibis.Compile.PolyQuantity (
    PolyQuantity,
    fromRawQuantity,
    toMass,
    toEnergy,
    toIU,
) where

import Crom.Units ((*.))
import qualified Crom.Units as U
import qualified Data.Text as T
import qualified Ibis.Read as R

-- | Quantities as present in the JSON food databases
data PolyQuantity
    = Mass U.Mass
    | Energy U.Energy
    | SpecificGravity Float
    | InternationalUnit Float
    | Unknown T.Text Float

fromRawQuantity :: R.RawQuantity -> PolyQuantity
fromRawQuantity (R.RawQuantity unit x) = case unit of
    "IU" -> InternationalUnit x
    "g" -> Mass $ x *. U.g
    "kJ" -> Energy $ x *. U.kJ
    "kcal" -> Energy $ x *. U.kcal
    "mg" -> Mass $ x *. U.mg
    "mg_ATE" -> Mass $ x *. U.mg -- Milligrams of Alpha-Tocopherol Equivalents (Vitamin E)
    "sp gr" -> SpecificGravity x -- Density relative to water: https://en.wikipedia.org/wiki/Relative_density
    "µg" -> Mass $ x *. U.ug
    _ -> Unknown unit x

toMass :: PolyQuantity -> Maybe U.Mass
toMass (Mass x) = Just x
toMass _ = Nothing

toEnergy :: PolyQuantity -> Maybe U.Energy
toEnergy (Energy x) = Just x
toEnergy _ = Nothing

toIU :: PolyQuantity -> Maybe Float
toIU (InternationalUnit x) = Just x
toIU _ = Nothing
