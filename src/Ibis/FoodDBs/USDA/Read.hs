{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Module for reading JSON food databases.
module Ibis.FoodDBs.USDA.Read (
    JSFood (..),
    JSNutrient (..),
    JSNutrientInner (..),
    RawFood (..),
    RawFoodID (..),
    RawFoodData (..),
    rawFoodParseErrs,
) where

import Control.Applicative ((<|>))
import Control.Monad (mfilter)
import Crom.Units ((*.))
import Crom.Units qualified as U
import Data.Aeson.Types qualified as J
import Data.Text qualified as T
import GHC.Generics
import Ibis.Data.MultiMap qualified as MM
import Ibis.FoodDBs.USDA.Compile.PolyQuantity qualified as P

-- | Food data as represented in the JSON databases.
data JSFood = JSFood
    { description :: T.Text,
      brandName :: Maybe T.Text,
      subbrandName :: Maybe T.Text,
      foodNutrients :: [JSNutrient]
    }
    deriving (Show, Generic, J.FromJSON)

-- Use Maybe fields to prevent the whole parse from failing
-- if either 'amount' or 'median' field is missing.
data JSNutrient = JSNutrient
    { amount :: Maybe Float,
      median :: Maybe Float,
      nutrient :: JSNutrientInner
    }
    deriving (Show, Generic, J.FromJSON)

data JSNutrientInner = JSNutrientInner
    { name :: T.Text,
      unitName :: T.Text
    }
    deriving (Show, Generic, J.FromJSON)

data RawFood = RawFood
    { foodID :: RawFoodID,
      foodData :: RawFoodData
    }

data RawFoodID = RawFoodID
    { name :: T.Text,
      brand :: T.Text, -- Empty if field is absent
      subbrand :: T.Text -- Empty if field is absent
    }
    deriving (Show, Eq, Ord)

data RawFoodData = RawFoodData
    { massFracs :: MM.MultiMap T.Text U.MassFraction,
      energyDensities :: MM.MultiMap T.Text U.EnergyDensity,
      ius :: MM.MultiMap T.Text Float,
      unknown :: [(T.Text, T.Text, Float)],
      malformed :: [T.Text]
    }

rawFoodParseErrs :: RawFood -> T.Text
rawFoodParseErrs (RawFood foodID foodData)
    | null allErrs = mempty
    | otherwise = T.unlines $ T.show foodID : allErrs
  where
    indent8 :: T.Text -> T.Text
    indent8 = ("        " <>)
    malformed =
        if null foodData.malformed
            then []
            else "    nutrients missing both 'amount' and 'median' fields:" : (indent8 <$> foodData.malformed)
    unknown =
        if null foodData.unknown
            then []
            else "    nutrients with uknown units:" : (indent8 . T.show <$> foodData.unknown)
    allErrs = malformed ++ unknown

instance J.FromJSON RawFood where
    parseJSON x = do
        foodID <- J.parseJSON x
        foodData <- J.parseJSON x
        return RawFood{foodID, foodData}

instance J.FromJSON RawFoodID where
    parseJSON = fmap toRawFoodID . J.parseJSON
      where
        toRawFoodID :: JSFood -> RawFoodID
        toRawFoodID x =
            RawFoodID
                { name = T.strip x.description,
                  brand = maybe "" T.strip x.brandName,
                  subbrand = maybe "" T.strip x.subbrandName
                }

instance J.FromJSON RawFoodData where
    parseJSON = fmap toRawFoodData . J.parseJSON
      where
        toRawFoodData :: JSFood -> RawFoodData
        toRawFoodData = sortNutrients . fmap fromJSNutrient . (.foodNutrients)
        fromJSNutrient :: JSNutrient -> (T.Text, P.PolyQuantity)
        fromJSNutrient n = (name, maybe P.Malformed (parseQty unit) amount)
          where
            name = T.strip n.nutrient.name :: T.Text
            -- Negative amounts are likely errors.
            -- They would only sense for anti-nutrients, but I doubt the USDA dataset handles them this way.
            -- Ideally they would be reported as errors, but silently discarding them suffices.
            amount = (mfilter (>= 0) n.amount) <|> (mfilter (>= 0) n.median) :: Maybe Float
            unit = T.strip n.nutrient.unitName :: T.Text
        sortNutrients :: [(T.Text, P.PolyQuantity)] -> RawFoodData
        sortNutrients = foldr f (RawFoodData mempty mempty mempty mempty mempty)
          where
            f :: (T.Text, P.PolyQuantity) -> RawFoodData -> RawFoodData
            f (k, P.MassFraction x) s = s{massFracs = MM.insert k x s.massFracs}
            f (k, P.EnergyDensity x) s = s{energyDensities = MM.insert k x s.energyDensities}
            f (_, P.SpecificGravity _) s = s -- We don't care about specific gravity
            f (k, P.InternationalUnit x) s = s{ius = MM.insert k x s.ius}
            f (k, P.Unknown u x) s = s{unknown = (k, u, x) : s.unknown}
            f (k, P.Malformed) s = s{malformed = k : s.malformed}
        -- Parse quantities from the JSON food database.
        -- They are given in absolute units (e.g. g, mg, kJ), but signify that amount per 100g.
        parseQty :: T.Text -> Float -> P.PolyQuantity
        parseQty unit x = case unit of
            "IU" -> P.InternationalUnit x -- Per 100g.
            "g" -> P.MassFraction $ x *. U.g_per_100g
            "kJ" -> P.EnergyDensity $ x *. U.kJ_per_100g
            "kcal" -> P.EnergyDensity $ x *. U.kcal_per_100g
            "mg" -> P.MassFraction $ x *. U.mg_per_100g
            "mg_ATE" -> P.MassFraction $ x *. U.mg_per_100g -- Milligrams of Alpha-Tocopherol Equivalents (Vitamin E)
            "sp gr" -> P.SpecificGravity x -- Density relative to water: https://en.wikipedia.org/wiki/Relative_density
            "µg" -> P.MassFraction $ x *. U.ug_per_100g
            _ -> P.Unknown unit x
