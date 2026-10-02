module Crom.Food (
    FoodID (..),
    FoodData (..),
    FoodDB,
    derive_season,
    derive_calories,
    derive_nutrients,
) where

import Crom.Calendar
import Crom.Nutrients (Nutrient)
import Crom.Units
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T

data FoodID = FoodID
    { name :: T.Text,
      brand :: T.Text,
      subbrand :: T.Text
    }
    deriving (Eq, Ord, Show, Read)

-- Generic food, ingredients, or meal/recipe, not tied to a specific product or manufacturer.
-- E.g. potatoes, eggs, protein powder, tofu, chocolate, vegetable paella, carbonara..
data FoodData = FoodData
    { edible :: Bool, -- If it's edible as-is, e.g. bread. Otherwise it should be an ingredient to a different food, e.g. tomato sauce or raw potatoes.
    -- Used to derive nutrient content. Can be empty, usually for basic ingredients (tomato, flour, pasta..)
      ingredients :: M.Map FoodID MassFraction,
      recipe :: T.Text, -- Empty if no preparation is necessary.
      season :: S.Set Month, -- Which months of the year the food is in season.
      calories :: Maybe EnergyDensity, -- Net, after discarding inedible parts. 'Maybe' to handle missing data.
      -- Net, after discarding inedible parts.
      -- Not necessarily the sum of ingredients, as nutrients can be altered by cooking (e.g. heat destroys vitamin C).
      nutrients :: M.Map Nutrient MassFraction
    }
    deriving (Show, Read)

type FoodDB = M.Map FoodID FoodData

-- Months during which all ingredients are in season.
derive_season :: (Foldable f) => f FoodData -> S.Set Month
derive_season = foldr (S.intersection . (.season)) perennial

-- Calory density of combined ingredients.
derive_calories :: [(FoodData, MassFraction)] -> EnergyDensity
derive_calories = sumQty . fmap (\(food, frac) -> get_calories food .*. frac)
  where
    get_calories :: FoodData -> EnergyDensity
    get_calories = fromMaybe zeroQty . (.calories)

-- Nutrient fractions of combined ingredients.
derive_nutrients :: [(FoodData, MassFraction)] -> M.Map Nutrient MassFraction
derive_nutrients =
    -- Convert individual foods to their nutrient fractions, then sum.
    foldr (M.unionWith (.+.) . nutrients_in_food) M.empty
  where
    -- Nutrient fractions in one food fraction.
    nutrients_in_food :: (FoodData, MassFraction) -> M.Map Nutrient MassFraction
    nutrients_in_food (food, frac) = (frac .*.) <$> food.nutrients
