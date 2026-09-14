{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | Module for reading JSON food databases.
module Ibis.Read (
    RawFood (..),
    RawNutrient (..),
    RawQuantity (..),
) where

import qualified Data.Aeson.Types as J
import qualified Data.Text as T
import GHC.Generics

-- | Food data as represented in the JSON databases.
data JSFood = JSFood
    { description :: T.Text,
      foodNutrients :: [JSNutrient]
    }
    deriving (Show, Generic)

data JSNutrient = JSNutrient
    { amount :: Float,
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

-- | Re-shaped and stripped of whitespace, but otherwise unprocessed food data.
data RawFood = RawFood
    { name :: T.Text,
      nutrients :: [RawNutrient]
    }

data RawNutrient = RawNutrient
    { name :: T.Text,
      quantity :: RawQuantity
    }

data RawQuantity = RawQuantity
    { unit :: T.Text,
      amount :: Float
    }

instance J.FromJSON RawFood where
    parseJSON = fmap toRawFood . J.parseJSON
      where
        toRawFood :: JSFood -> RawFood
        toRawFood x = RawFood (T.strip x.description) (toRawNutrient <$> x.foodNutrients)
        toRawNutrient :: JSNutrient -> RawNutrient
        toRawNutrient x =
            RawNutrient
                (T.strip x.nutrient.name)
                (RawQuantity (T.strip x.nutrient.unitName) x.amount)
