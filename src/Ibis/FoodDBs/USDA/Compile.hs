-- | Convert raw food data into useful form.
module Ibis.FoodDBs.USDA.Compile (
    toFood,
) where

import Crom.Calendar qualified as C
import Crom.Food qualified as F
import Crom.Units qualified as U
import Data.Bifunctor (bimap)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Ibis.Data.MultiMap qualified as MM
import Ibis.FoodDBs.USDA.Compile.Nutrients qualified as N
import Ibis.FoodDBs.USDA.Read qualified as R

toFoodID :: R.RawFoodID -> F.FoodID
toFoodID rawFood =
    F.FoodID
        { name = rawFood.name,
          brand = rawFood.brand,
          subbrand = rawFood.subbrand
        }

-- If the same food has multiple entries, average its values together.
combineFoodData :: (Foldable f, Functor f) => f R.RawFoodData -> R.RawFoodData
combineFoodData xs =
    R.RawFoodData
        { massFracs = combineMMs $ (.massFracs) <$> xs,
          energyDensities = combineMMs $ (.energyDensities) <$> xs,
          ius = combineMMs $ (.ius) <$> xs,
          unknown = concatMap (.unknown) xs,
          malformed = concatMap (.malformed) xs
        }
  where
    -- Merge all the multimaps, and produce an average for each key.
    combineMMs :: (Foldable f, Ord k, U.QDiv a Float a, U.QSum a) => f (MM.MultiMap k a) -> MM.MultiMap k a
    combineMMs = fmap (NE.singleton . U.avgQtyNE) . M.unionsWith (<>)

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

toFood :: (Foldable f, Functor f) => (R.RawFoodID, f R.RawFoodData) -> (F.FoodID, F.FoodData)
toFood = bimap toFoodID (toFoodData . combineFoodData)
