-- | Convert raw food data into useful form.
module Ibis.FoodDBs.USDA.Compile (
    toFood,
) where

import Crom.Calendar qualified as C
import Crom.Food qualified as F
import Ibis.FoodDBs.USDA.Compile.Nutrients qualified as N
import Ibis.FoodDBs.USDA.Read qualified as R

toFoodID :: R.RawFoodID -> F.FoodID
toFoodID rawFood =
    F.FoodID
        { name = rawFood.name,
          brand = rawFood.brand,
          subbrand = rawFood.subbrand
        }

toFoodData :: R.RawFoodData -> F.FoodData
toFoodData rawFood =
    F.FoodData
        { edible = True,
          ingredients = mempty,
          recipe = mempty,
          season = C.perennial,
          calories = N.deriveCalories rawFood,
          nutrients = N.deriveNutrients rawFood
        }

toFood :: R.RawFood -> (F.FoodID, F.FoodData)
toFood raw = (toFoodID raw.foodID, toFoodData raw.foodData)
