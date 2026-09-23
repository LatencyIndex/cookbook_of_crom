-- | Convert raw food data into useful form.
module Ibis.FoodDBs.USDA.Compile (
    toFood,
) where

import qualified Crom.Food as F
import qualified Ibis.FoodDBs.USDA.Read as R

toFood :: R.RawFood -> F.Food
toFood = undefined -- TODO
