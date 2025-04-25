module Crom.Food where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Crom.Nutrients as N
import qualified Crom.Units as U

-- Generic food or ingredients, not tied to a specific product or manufacturer.
-- E.g. potatoes, eggs, protein powder, tofu, chocolate..
data Food = Food {
    name :: T.Text,
    nutrients_per_100g :: M.Map N.Nutrient U.Mass,
    calories_per_100g :: U.Energy,
    edible_fraction :: Float, -- What remains after discarding waste parts, by e.g. peeling.
    edible :: Bool -- If it's edible as-is, e.g. bread. Otherwise it should be an ingredient to a recipe, e.g. tomato sauce or raw potatoes.
} deriving (Show)

-- Amount of nutrients in a certain mass of an ingredient.
nutrientsIn :: U.Mass -> Food -> M.Map N.Nutrient U.Mass
nutrientsIn mass = fmap (nb_100g U..*) . nutrients_per_100g
    where nb_100g = mass U../ U.as_g 100
