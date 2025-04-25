module Crom.Food where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Function (on)
import Crom.Nutrients (Nutrient)
import Crom.Units

-- Generic food, ingredients, or meal/recipe, not tied to a specific product or manufacturer.
-- E.g. potatoes, eggs, protein powder, tofu, chocolate, vegetable paella, carbonara..
data Food = Food {
    name :: T.Text,
    edible :: Bool, -- If it's edible as-is, e.g. bread. Otherwise it should be an ingredient to a different food, e.g. tomato sauce or raw potatoes.
    -- Used to derive nutrient content. Can be empty, usually for basic ingredients (tomato, flour, pasta..)
    ingredients :: M.Map Food MassFraction,
    recipe :: T.Text, -- Empty if no preparation is necessary.
    calories :: EnergyDensity, -- Net, after discarding inedible parts.
    -- Net, after discarding inedible parts.
    -- Not necessarily the sum of ingredients, as nutrients can be altered by cooking (e.g. heat destroys vitamin C).
    nutrients :: M.Map Nutrient MassFraction
} deriving (Show)

instance Eq Food where (==) = (==) `on` name
instance Ord Food where compare = compare `on` name

-- Nutrient fractions of combined ingredients.
nutrients_in_ingredients :: M.Map Food MassFraction -> M.Map Nutrient MassFraction
nutrients_in_ingredients =
    foldr (M.unionWith (.+.)) M.empty . -- Sum nutrients from each food.
    M.mapWithKey nutrients_in_food -- Nutrient fractions in an individual food.
    where
    -- Nutrient fractions in one food fraction.
    nutrients_in_food :: Food -> MassFraction -> M.Map Nutrient MassFraction
    nutrients_in_food food frac = (frac .*.) <$> nutrients food
