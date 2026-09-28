module Crom.Food (
    Food (..),
    derive_season,
    derive_calories,
    derive_nutrients,
) where

import Crom.Calendar
import Crom.Nutrients (Nutrient)
import Crom.Units
import Data.Function (on)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T

-- Generic food, ingredients, or meal/recipe, not tied to a specific product or manufacturer.
-- E.g. potatoes, eggs, protein powder, tofu, chocolate, vegetable paella, carbonara..
data Food = Food
    { name :: T.Text,
      edible :: Bool, -- If it's edible as-is, e.g. bread. Otherwise it should be an ingredient to a different food, e.g. tomato sauce or raw potatoes.
      -- Used to derive nutrient content. Can be empty, usually for basic ingredients (tomato, flour, pasta..)
      ingredients :: M.Map Food MassFraction,
      recipe :: T.Text, -- Empty if no preparation is necessary.
      season :: S.Set Month, -- Which months of the year the food is in season.
      calories :: Maybe EnergyDensity, -- Net, after discarding inedible parts. 'Maybe' to handle missing data.
      -- Net, after discarding inedible parts.
      -- Not necessarily the sum of ingredients, as nutrients can be altered by cooking (e.g. heat destroys vitamin C).
      nutrients :: M.Map Nutrient MassFraction
    }
    deriving (Show)

instance Eq Food where (==) = (==) `on` (.name)
instance Ord Food where compare = compare `on` (.name)

-- Months during which all ingredients are in season.
derive_season :: M.Map Food MassFraction -> S.Set Month
derive_season = foldr (S.intersection . (.season)) perennial . M.keys

-- Calory density of combined ingredients.
derive_calories :: M.Map Food MassFraction -> EnergyDensity
derive_calories = mconcat . M.elems . M.mapWithKey (\food frac -> get_calories food .*. frac)
  where
    get_calories :: Food -> EnergyDensity
    get_calories = fromMaybe mempty . (.calories)

-- Nutrient fractions of combined ingredients.
derive_nutrients :: M.Map Food MassFraction -> M.Map Nutrient MassFraction
derive_nutrients =
    foldr (M.unionWith (.+.)) M.empty -- Sum nutrients from each food.
        . M.mapWithKey nutrients_in_food -- Nutrient fractions in an individual food.
  where
    -- Nutrient fractions in one food fraction.
    nutrients_in_food :: Food -> MassFraction -> M.Map Nutrient MassFraction
    nutrients_in_food food frac = (frac .*.) <$> food.nutrients
