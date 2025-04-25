module Crom.Recipes where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Crom.Ingredients as I
import qualified Crom.Nutrients as N
import qualified Crom.Units as U

data Recipe = Recipe {
    name :: T.Text,
    instructions :: T.Text,
    ingredients :: [(I.Ingredient, U.Mass)], -- Quantities for a single serving. Usable fraction only.
    preparation_mins :: Float, -- Active preparation time, excluding unattended parts.
    persistence_days :: Float -- How long the meal keeps, in fridge or freezer.
} deriving (Show)

nutrientsPerServing :: Recipe -> M.Map N.Nutrient U.Mass
nutrientsPerServing
    = M.unionsWith U.qSum
    . fmap (uncurry $ flip I.nutrientsIn)
    . ingredients
