module Ibis.FoodDBs.USDA.Compile.PolyQuantity (
    PolyQuantity,
    fromRawQuantity,
    toMassFraction,
    toEnergyDensity,
    toIU,
) where

import Crom.Units ((*.))
import qualified Crom.Units as U
import qualified Data.Text as T
import qualified Ibis.FoodDBs.USDA.Read as R

-- | Quantities as present in the JSON food databases
data PolyQuantity
    = MassFraction U.MassFraction
    | EnergyDensity U.EnergyDensity
    | SpecificGravity Float
    | InternationalUnit Float
    | Unknown T.Text Float
    | Malformed -- Quantities missing both 'amount' and 'median' fields.

-- | Parse quantities from the JSON food database.
-- They are given in absolute units (e.g. g, mg, kJ), but signify that amount per 100g.
fromRawQuantity :: R.RawQuantity -> PolyQuantity
fromRawQuantity (R.RawQuantity unit (Just x)) = case unit of
    "IU" -> InternationalUnit x -- Also per 100g.
    "g" -> MassFraction $ x *. U.g_per_100g
    "kJ" -> EnergyDensity $ x *. U.kJ_per_100g
    "kcal" -> EnergyDensity $ x *. U.kcal_per_100g
    "mg" -> MassFraction $ x *. U.mg_per_100g
    "mg_ATE" -> MassFraction $ x *. U.mg_per_100g -- Milligrams of Alpha-Tocopherol Equivalents (Vitamin E)
    "sp gr" -> SpecificGravity x -- Density relative to water: https://en.wikipedia.org/wiki/Relative_density
    "µg" -> MassFraction $ x *. U.ug_per_100g
    _ -> Unknown unit x
fromRawQuantity (R.RawQuantity _ Nothing) = Malformed

toMassFraction :: PolyQuantity -> Maybe U.MassFraction
toMassFraction (MassFraction x) = Just x
toMassFraction _ = Nothing

toEnergyDensity :: PolyQuantity -> Maybe U.EnergyDensity
toEnergyDensity (EnergyDensity x) = Just x
toEnergyDensity _ = Nothing

toIU :: PolyQuantity -> Maybe Float
toIU (InternationalUnit x) = Just x
toIU _ = Nothing
