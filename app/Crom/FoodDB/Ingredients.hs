module Crom.FoodDB.Ingredients where

import Crom.Food
import Crom.Calendar
import qualified Data.Map as M

-- Placeholder example
carrots :: Food
carrots = Food {
    name = "carrots",
    edible = True,
    ingredients = M.empty,
    recipe = "",
    season = perennial,
    calories = undefined,
    nutrients = undefined
}
