module Crom.Products where

import qualified Data.Text as T
import qualified Crom.Ingredients as I
import qualified Crom.Time as Time

data Price = Price {
    date :: Time.Date,
    location :: T.Text,
    euros :: Float
} deriving (Show)

-- A specific item that can be bought in a store, containing one or more ingredients.
-- E.g. Zelene Doline 2 litre yoghurt, Polonca potatoes, Milka 250g milk chocolate..
data Product = Product {
    name :: T.Text,
    ingredients :: [(I.Ingredient, Float)],  -- Ingredient quantities, in mass fractions
    grams :: Float,
    prices :: [Price]  -- Price history
} deriving (Show)
