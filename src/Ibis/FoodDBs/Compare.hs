-- | For comparing the USDA and GNUtrition databases for differences or errors.
module Ibis.FoodDBs.Compare (
    compareDBs,
    printDelta,
) where

import Crom.Food qualified as F
import Crom.Nutrients qualified as N
import Crom.Units ((*.), (.*), (.*.), (./.))
import Crom.Units qualified as U
import Crom.Util.Text (PrettyShow (..), showSigDigits)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Ibis.FoodDBs.GNUtrition.DBColumns as Cols
import Ibis.FoodDBs.GNUtrition.Read qualified as G

data MassDelta = MassDelta
    { left :: U.MassFraction,
      right :: U.MassFraction,
      relDelta :: Float
    }
instance PrettyShow MassDelta where
    -- Omit the "/ 100g" part of the unit, for less visual clutter.
    prettyShow x =
        prettyShow (x.left .*. U.g .* 100)
            ++ ", "
            ++ prettyShow (x.right .*. U.g .* 100)
            ++ ", "
            ++ showSigDigits 2 x.relDelta

type NutrientDeltas = M.Map N.Nutrient MassDelta
type NutrientMap = M.Map N.Nutrient U.MassFraction

-- | Translate from GNUtrition's nutrients, to Crom nutrients.
toCromNutrients :: M.Map T.Text U.MassFraction -> NutrientMap
toCromNutrients xs = M.mapMaybe wsum Cols.equivalences
  where
    -- Lookup mass fraction and apply weight in one step.
    lookupW :: (Float, T.Text) -> Maybe U.MassFraction
    lookupW (w, k) = (w *.) <$> M.lookup k xs
    -- Weighted sum of GNUtrition nutrients in xs. Nothing for empty sums.
    wsum :: [(Float, T.Text)] -> Maybe U.MassFraction
    wsum wks = case mapMaybe lookupW wks of
        [] -> Nothing
        ms -> Just $ U.sumQty ms

relativeDelta :: Float -> Float -> Maybe Float
relativeDelta x y
    | delta > 0 = Just $ delta / max x y
    | otherwise = Nothing
  where
    delta = abs (x - y)

-- | The original values, and their relative difference, if > 0.
cmpMassFracs :: U.MassFraction -> U.MassFraction -> Maybe MassDelta
cmpMassFracs x y = MassDelta x y <$> relativeDelta (x ./. U.g_per_100g) (y ./. U.g_per_100g)

-- | List all the nutrients that differ by more than tolerance (tol).
-- E.g. if tol = 0.02, they must differ by > 2%
cmpNutrients :: Float -> NutrientMap -> NutrientMap -> M.Map N.Nutrient MassDelta
cmpNutrients tol l r = M.filter (\x -> x.relDelta > tol) $ M.mapMaybe id $ M.intersectionWith cmpMassFracs l r

cmpFoods :: Float -> F.FoodData -> G.GNUFoodData -> M.Map N.Nutrient MassDelta
cmpFoods tol usda gnu = cmpNutrients tol usda.nutrients (toCromNutrients gnu.nutrients)

-- | Match foods by name, and compare their nutrient content.
-- Nutrients with relative difference < 'tol' are considered equivalent, and omitted from the output.
compareDBs :: Float -> F.FoodDB -> G.GNUDB -> M.Map T.Text NutrientDeltas
compareDBs tol usda gnu = M.filter (not . M.null) $ M.intersectionWith (cmpFoods tol) usdaByName gnuByName
  where
    usdaByName = M.mapKeys (.name) usda
    gnuByName = M.mapKeys (.name) gnu

-- | Pretty-print the difference of nutrient content in a food.
printDelta :: (T.Text, NutrientDeltas) -> IO ()
printDelta (foodName, xs) = do
    TIO.putStrLn foodName
    mapM_ (\(n, delta) -> putStrLn ("    " ++ show n ++ ": " ++ prettyShow delta)) (M.toList xs)
