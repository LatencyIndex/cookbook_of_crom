module Crom.Food where

import qualified Data.Map as M
import qualified Data.Text as T
import Crom.Nutrients (Nutrient)
import Crom.Units.Energy (Energy)
import Crom.Units.Mass (Mass, as_g)
import Crom.Units.Units ((./), (.*))

-- Generic food or ingredients, not tied to a specific product or manufacturer.
-- E.g. potatoes, eggs, protein powder, tofu, chocolate..
data Food = Food {
    name :: T.Text,
    nutrients_per_100g :: M.Map Nutrient Mass,
    calories_per_100g :: Energy,
    edible_fraction :: Float, -- What remains after discarding waste parts, by e.g. peeling.
    edible :: Bool -- If it's edible as-is, e.g. bread. Otherwise it should be an ingredient to a recipe, e.g. tomato sauce or raw potatoes.
} deriving (Show)

-- Amount of nutrients in a certain mass of an ingredient.
nutrientsIn :: Mass -> Food -> M.Map Nutrient Mass
nutrientsIn mass = fmap (nb_100g .*) . nutrients_per_100g
    where nb_100g = mass ./ as_g 100
