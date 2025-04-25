module Crom.Recipes where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Crom.Food as F
import qualified Crom.Nutrients as N
import qualified Crom.Units as U

data Recipe = Recipe {
    result :: F.Food,
    ingredients :: [(F.Food, U.Mass)], -- Quantities for a single serving. Usable fraction only.
    instructions :: T.Text,
    preparation_time :: U.Duration, -- Active preparation time, excluding unattended parts.
    persistence_time :: U.Duration -- How long the meal keeps, in fridge or freezer.
} deriving (Show)

nutrientsPerServing :: Recipe -> M.Map N.Nutrient U.Mass
nutrientsPerServing
    = M.unionsWith U.qSum
    . fmap (uncurry $ flip F.nutrientsIn)
    . ingredients
