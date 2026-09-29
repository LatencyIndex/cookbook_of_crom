-- | Convert raw food data into useful form.
module Ibis.FoodDBs.USDA.Compile (
    toFood,
) where

import Crom.Calendar qualified as C
import Crom.Food qualified as F
import Ibis.FoodDBs.USDA.Compile.Nutrients qualified as N
import Ibis.FoodDBs.USDA.Read qualified as R

toFoodID :: R.RawFood -> F.FoodID
toFoodID rawFood =
    F.FoodID
        { name = rawFood.name,
          brand = rawFood.brand,
          subbrand = rawFood.subbrand
        }

toFoodData :: R.RawFood -> F.FoodData
toFoodData rawFood =
    F.FoodData
        { edible = True,
          ingredients = mempty,
          recipe = mempty,
          season = C.perennial,
          calories = N.deriveCalories nutrientMap,
          nutrients = N.deriveNutrients nutrientMap
        }
  where
    nutrientMap :: N.JSNutrientMap
    nutrientMap = N.toJSNutrientMap rawFood.nutrients

toFood :: R.RawFood -> (F.FoodID, F.FoodData)
toFood x = (toFoodID x, toFoodData x)
