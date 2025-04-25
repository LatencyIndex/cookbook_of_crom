module Crom.Shopping where

import qualified Data.Text as T
import Crom.Food (Food)
import Crom.Time (Date)

data Price = Price {
    date :: Date,
    location :: T.Text,
    euros :: Float
} deriving (Show)

-- A specific item that can be bought in a store, containing one or more ingredients.
-- E.g. Zelene Doline 2 litre yoghurt, Polonca potatoes, Milka 250g milk chocolate..
data Product = Product {
    name :: T.Text,
    ingredients :: [(Food, Float)],  -- Ingredient quantities, in mass fractions
    grams :: Float,
    prices :: [Price]  -- Price history
} deriving (Show)
