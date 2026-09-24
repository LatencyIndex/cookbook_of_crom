-- | Convert raw food data into useful form.
module Ibis.FoodDBs.USDA.Compile (
    toFood,
) where

import qualified Crom.Calendar as C
import qualified Crom.Food as F
import qualified Ibis.FoodDBs.USDA.Compile.Nutrients as N
import qualified Ibis.FoodDBs.USDA.Read as R

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
