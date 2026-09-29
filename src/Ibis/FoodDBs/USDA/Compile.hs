-- | Convert raw food data into useful form.
module Ibis.FoodDBs.USDA.Compile (
    toFood,
) where

import Crom.Calendar qualified as C
import Crom.Food qualified as F
import Ibis.FoodDBs.USDA.Compile.Nutrients qualified as N
import Ibis.FoodDBs.USDA.Read qualified as R

toFood :: R.RawFood -> F.Food
toFood rawFood =
    F.Food
        { name = rawFood.name,
          edible = True,
          ingredients = mempty,
          recipe = mempty,
          season = C.perennial,
          calories = N.deriveCalories nutrientMap,
          nutrients = N.deriveNutrients nutrientMap
        }
  where
    nutrientMap :: N.JSNutrientMap
    nutrientMap = N.toJSNutrientMap rawFood.nutrients
