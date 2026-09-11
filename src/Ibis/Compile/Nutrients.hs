module Ibis.Compile.Nutrients (

) where

import qualified Crom.Nutrients as N
import qualified Crom.Units as U
import qualified Data.Map as M
import qualified Ibis.Read as R

deriveNutrients :: R.RawFood -> M.Map N.Nutrient U.MassFraction
deriveNutrients = undefined -- TODO

deriveCalories :: R.RawFood -> Maybe U.EnergyDensity
deriveCalories = undefined -- TODO
