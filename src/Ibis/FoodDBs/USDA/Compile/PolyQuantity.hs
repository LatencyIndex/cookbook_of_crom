module Ibis.FoodDBs.USDA.Compile.PolyQuantity (
    PolyQuantity (..),
) where

import Crom.Units qualified as U
import Data.Text qualified as T

-- | Quantities as present in the JSON food databases
data PolyQuantity
    = MassFraction U.MassFraction
    | EnergyDensity U.EnergyDensity
    | SpecificGravity Float
    | InternationalUnit Float
    | Unknown T.Text Float
    | Malformed -- Quantities missing both 'amount' and 'median' fields.
