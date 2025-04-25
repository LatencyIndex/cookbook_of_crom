module Crom.FoodDB.Ingredients where

import qualified Data.Map as M
import Crom.Food
import qualified Crom.Calendar as C

-- Placeholder example
carrots :: Food
carrots = Food {
    name = "carrots",
    edible = True,
    ingredients = M.empty,
    recipe = "",
    season = C.perennial,
    calories = undefined,
    nutrients = undefined
}
