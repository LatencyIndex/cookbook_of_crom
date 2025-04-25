-- Module for processing json data from https://fdc.nal.usda.gov/download-datasets.html
-- Mild explanation of data format: https://fdc.nal.usda.gov/portal-data/external/dataDictionary
module Crom.FoodDB.Parsing.USDA (
    prettify,
    simplify
) where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as J
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.KeyMap as K
import Data.Scientific (toRealFloat)
import qualified Data.Text as T

loadJson :: FilePath -> IO J.Value
loadJson path = fromJust . J.decode <$> BL.readFile path

prettify :: FilePath -> FilePath -> IO ()
prettify inp out = loadJson inp >>= BL.writeFile out . encodePretty

-- Want to keep only nodes we care about, discard the rest. We care about:
    -- Food nodes: carrots, rice, hummus, etc. The things that the analyses refer to.
        -- Identified by:
            -- Is in the root of "FoundationFoods" or "SurveyFoods" J.Array
            -- Is an Object
            -- Has "fdcId" and "foodNutrients" keys
        -- Keys we care about:
            -- "description" - value is Text. Name of the food.
            -- "foodNutrients" - value is Object. Nutrient content.
    -- Nutrient content: how much of which nutrient the parent food has.
        -- Identified by:
            -- In the Array stored under "foodNutrients" key of a food node.
            -- Is an Object, with "amount" and "nutrient" keys.
            -- Under "nutrient" is another Object, with "name" and "unitName" keys.
        -- Keys we care about:
            -- "amount" - value is Number
            -- "nutrient" - value is Object. Nested keys we care about:
                -- "name" - value is Text
                -- "unitName" -- value is Text

data Nutrient = Nutrient {
    name :: T.Text,
    amount :: Float,
    unit :: T.Text
}
instance Show Nutrient where
    show (Nutrient name amount unit) = T.unpack name ++ ": " ++ show amount ++ " " ++ T.unpack unit

data Food = Food {
    name :: T.Text,
    nutrients :: [Nutrient]
}
instance Show Food where
    show (Food name nutrients) = T.unpack name ++ "\n" ++ unlines (("    " ++) . show <$> nutrients)

asArray :: J.Value -> Maybe J.Array
asArray (J.Array x) = Just x
asArray _ = Nothing

asNumber :: J.Value -> Maybe Float
asNumber (J.Number x) = Just $ toRealFloat x
asNumber _ = Nothing

asText :: J.Value -> Maybe T.Text
asText (J.String x) = Just x
asText _ = Nothing

jlookup :: J.Key -> J.Value -> Maybe J.Value
jlookup k (J.Object x) = K.lookup k x
jlookup _ _ = Nothing

-- To be applied on the elements in a FoundationFood's "foodNutrients" field.
toNutrient :: J.Value -> Maybe Nutrient
toNutrient d = Nutrient <$> name <*> amount <*> unit where
    amount = jlookup "amount" d >>= asNumber :: Maybe Float
    nutrient = jlookup "nutrient" d
    name = nutrient >>= jlookup "name" >>= asText :: Maybe T.Text
    unit = nutrient >>= jlookup "unitName" >>= asText :: Maybe T.Text

-- To be applied to the elements in the "FoundationFoods" Array.
toFood :: J.Value -> Maybe Food
toFood d = Food <$> name <*> nutrients'' where
    name = jlookup "description" d >>= asText :: Maybe T.Text
    nutrients = jlookup "foodNutrients" d >>= asArray :: Maybe J.Array
    nutrients' = V.mapMaybe toNutrient <$> nutrients :: Maybe (V.Vector Nutrient)
    nutrients'' = V.toList <$> nutrients' :: Maybe [Nutrient]

readFoods :: J.Value -> Maybe [Food]
readFoods d = V.toList <$> foods' where
    -- If neither "FoundationFoods" nor "SurveyFoods" are not found, or are not an Array, best we can do is crash.
    foundationFoods = jlookup "FoundationFoods" d >>= asArray :: Maybe J.Array
    surveyFoods = jlookup "SurveyFoods" d >>= asArray :: Maybe J.Array
    foods = foundationFoods <|> surveyFoods :: Maybe J.Array
    foods' = V.mapMaybe toFood <$> foods :: Maybe (V.Vector Food)

simplify :: FilePath -> FilePath -> IO ()
simplify inp out = do
    foods <- fromJust . readFoods <$> loadJson inp
    let text = unlines $ show <$> foods :: String
    writeFile out text
