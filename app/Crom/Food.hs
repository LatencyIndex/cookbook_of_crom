module Crom.Food where

import qualified Data.Map as M
import qualified Data.Text as T
import Crom.Nutrients (Nutrient)
import Crom.Units.Energy (Energy)
import Crom.Units.Mass (Mass, as_g)
import Crom.Units.Units ((.+), (./), (.*))

-- Generic food, ingredients, or meal/recipe, not tied to a specific product or manufacturer.
-- E.g. potatoes, eggs, protein powder, tofu, chocolate, vegetable paella, carbonara..
data Food = Food {
    name :: T.Text,
    -- Per serving. Empty if food is a basic ingredient that you don't make out of other ingredients yourself.
    -- E.g. raw tomato, flour, bread, tempeh..
    ingredients :: M.Map Food Mass,
    recipe :: T.Text, -- Empty if no preparation is necessary.
    -- Net, after discarding inedible parts.
    -- Not necessarily the sum of ingredients, as nutrients can be altered by cooking.
    -- E.g. heat destroys vitamin C.
    nutrients_per_100g :: M.Map Nutrient Mass,
    calories_per_100g :: Energy, -- Net, after discarding inedible parts.
    edible :: Bool -- If it's edible as-is, e.g. bread. Otherwise it should be an ingredient to a different food, e.g. tomato sauce or raw potatoes.
} deriving (Show)

-- Absolute amount of nutrients in a certain mass of a food.
nutrients_in :: Food -> Mass -> M.Map Nutrient Mass
nutrients_in food mass = ((mass ./ as_g 100) .*) <$> nutrients_per_100g food

-- Absolute sum of nutrients in a collection of foods.
derive_nutrients :: M.Map Food Mass -> M.Map Nutrient Mass
derive_nutrients =
    foldr (M.unionWith (.+)) M.empty . -- Sum nutrients from each food.
    M.mapWithKey nutrients_in -- Nutrients in an individual food.
