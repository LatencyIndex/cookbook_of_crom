module Crom.Recipes where

import qualified Data.Map as M
import qualified Data.Text as T
import Crom.Food (Food, nutrients_in)
import Crom.Nutrients (Nutrient)
import Crom.Units.Duration (Duration)
import Crom.Units.Mass (Mass)
import Crom.Units.Units ((.+))

data Recipe = Recipe {
    result :: Food,
    ingredients :: [(Food, Mass)], -- Quantities for a single serving. Usable fraction only.
    instructions :: T.Text,
    preparation_time :: Duration, -- Active preparation time, excluding unattended parts.
    persistence_time :: Duration -- How long the meal keeps, in fridge or freezer.
} deriving (Show)

nutrients_per_serving :: Recipe -> M.Map Nutrient Mass
nutrients_per_serving
    = M.unionsWith (.+)
    . fmap (uncurry $ flip nutrients_in)
    . ingredients
