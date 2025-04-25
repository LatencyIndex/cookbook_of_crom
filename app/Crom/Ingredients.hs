module Crom.Ingredients where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Crom.Nutrients as N
import qualified Crom.Units as U

-- Generic ingredients, not tied to a specific product or manufacturer.
-- E.g. potatoes, eggs, protein powder, tofu, chocolate..
data Ingredient = Ingredient {
    name :: T.Text,
    nutrients_per_100g :: M.Map N.Nutrient U.Mass,
    calories_per_100g :: U.Energy,
    edible_fraction :: Float -- What remains after discarding waste parts, by e.g. peeling.
} deriving (Show)

-- Amount of nutrients in a certain mass of an ingredient.
nutrientsIn :: U.Mass -> Ingredient -> M.Map N.Nutrient U.Mass
nutrientsIn mass = fmap (U.qMul nb_100g) . nutrients_per_100g
    where nb_100g = mass `U.qDiv` U.as_g 100
