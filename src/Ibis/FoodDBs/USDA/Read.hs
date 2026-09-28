{-# LANGUAGE DeriveGeneric #-}

-- | Module for reading JSON food databases.
module Ibis.FoodDBs.USDA.Read (
    RawFood (..),
    RawNutrient (..),
    RawQuantity (..),
    listRawFoodErrs,
) where

import Control.Applicative ((<|>))
import qualified Data.Aeson.Types as J
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import GHC.Generics

-- | Food data as represented in the JSON databases.
data JSFood = JSFood
    { description :: T.Text,
      foodNutrients :: [JSNutrient]
    }
    deriving (Show, Generic)

-- Use Maybe fields to prevent the whole parse from failing
-- if either 'amount' or 'median' field is missing.
data JSNutrient = JSNutrient
    { amount :: Maybe Float,
      median :: Maybe Float,
      nutrient :: JSNutrientInner
    }
    deriving (Show, Generic)

data JSNutrientInner = JSNutrientInner
    { name :: T.Text,
      unitName :: T.Text
    }
    deriving (Show, Generic)

instance J.FromJSON JSFood
instance J.FromJSON JSNutrient
instance J.FromJSON JSNutrientInner

-- | Re-shaped and stripped of whitespace, but otherwise unprocessed USDA JSON food data.
data RawFood = RawFood
    { name :: T.Text,
      nutrients :: [RawNutrient]
    }
    deriving (Show)

data RawNutrient = RawNutrient
    { name :: T.Text,
      quantity :: RawQuantity
    }
    deriving (Show)

data RawQuantity = RawQuantity
    { unit :: T.Text,
      amount :: Maybe Float
    }
    deriving (Show)

-- | Report if any nutrient is missing both 'amount' and 'median' fields.
listRawFoodErrs :: RawFood -> [T.Text]
listRawFoodErrs food = mapMaybe getNutrientErr food.nutrients
  where
    getNutrientErr :: RawNutrient -> Maybe T.Text
    getNutrientErr nutrient = case nutrient.quantity.amount of
        Nothing ->
            Just $
                "Food "
                    <> food.name
                    <> "\n    nutrient "
                    <> nutrient.name
                    <> "\n    quantity error: missing both 'amount' and 'median' fields."
        Just _ -> Nothing

instance J.FromJSON RawFood where
    parseJSON = fmap toRawFood . J.parseJSON
      where
        toRawFood :: JSFood -> RawFood
        toRawFood x = RawFood (T.strip x.description) (toRawNutrient <$> x.foodNutrients)
        toRawNutrient :: JSNutrient -> RawNutrient
        toRawNutrient x =
            RawNutrient
                (T.strip x.nutrient.name)
                (RawQuantity (T.strip x.nutrient.unitName) (x.amount <|> x.median))
