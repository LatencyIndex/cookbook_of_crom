module Ibis.FoodDBs.GNUtrition.Read (
    loadGNU,
) where

import Control.Monad ((<=<))
import Crom.Units ((.*.))
import Crom.Units qualified as U
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Database.SQLite.Simple qualified as S
import Text.Read (readMaybe)

{- How to explore SQL databases:
Open the DB with
    > sqlite3 -readonly food.db
List all tables with
    > .tables
Show the column names and types of a table with
    > .schema <table name>
A slightly nicer list of column names:
    > PRAGMA table_info(table_name);
Yes, some column names contain line-breaks.
-}

type RowMap = M.Map T.Text T.Text

nameKey :: T.Text
nameKey = "Main food description"

caloriesKey :: T.Text
caloriesKey = "Energy (kcal)"

_miscKeys :: [T.Text]
_miscKeys =
    [ "Food code",
      "WWEIA Category number",
      "WWEIA Category description"
    ]

nutrientsKeys :: [T.Text]
nutrientsKeys =
    [ "Protein (g)",
      "Carbohydrate (g)",
      "Sugars, total\n(g)",
      "Fiber, total dietary (g)",
      "Total Fat (g)",
      "Fatty acids, total saturated (g)",
      "Fatty acids, total monounsaturated (g)",
      "Fatty acids, total polyunsaturated (g)",
      "Cholesterol (mg)",
      "Retinol (mcg)",
      "Vitamin A, RAE (mcg_RAE)",
      "Carotene, alpha (mcg)",
      "Carotene, beta (mcg)",
      "Cryptoxanthin, beta (mcg)",
      "Lycopene (mcg)",
      "Lutein + zeaxanthin (mcg)",
      "Thiamin (mg)",
      "Riboflavin (mg)",
      "Niacin (mg)",
      "Vitamin B-6 (mg)",
      "Folic acid (mcg)",
      "Folate, food (mcg)",
      "Folate, DFE (mcg_DFE)",
      "Folate, total (mcg)",
      "Choline, total (mg)",
      "Vitamin B-12 (mcg)",
      "Vitamin B-12, added\n(mcg)",
      "Vitamin C (mg)",
      "Vitamin D (D2 + D3) (mcg)",
      "Vitamin E (alpha-tocopherol) (mg)",
      "Vitamin E, added\n(mg)",
      "Vitamin K (phylloquinone) (mcg)",
      "Calcium (mg)",
      "Phosphorus (mg)",
      "Magnesium (mg)",
      "Iron\n(mg)",
      "Zinc\n(mg)",
      "Copper (mg)",
      "Selenium (mcg)",
      "Potassium (mg)",
      "Sodium (mg)",
      "Caffeine (mg)",
      "Theobromine (mg)",
      "Alcohol (g)",
      "4:0\n(g)",
      "6:0\n(g)",
      "8:0\n(g)",
      "10:0\n(g)",
      "12:0\n(g)",
      "14:0\n(g)",
      "16:0\n(g)",
      "18:0\n(g)",
      "16:1\n(g)",
      "18:1\n(g)",
      "20:1\n(g)",
      "22:1\n(g)",
      "18:2\n(g)",
      "18:3\n(g)",
      "18:4\n(g)",
      "20:4\n(g)",
      "20:5 n-3\n(g)",
      "22:5 n-3\n(g)",
      "22:6 n-3\n(g)",
      "Water\n(g)"
    ]

type GNUFoodID = T.Text

data GNUFoodData = GNUFoodData
    { calories :: Maybe U.EnergyDensity,
      nutrients :: M.Map T.Text U.MassFraction
    }
    deriving (Show)

parseGNUFoodID :: RowMap -> Maybe GNUFoodID
parseGNUFoodID = M.lookup nameKey

parseGNUFoodData :: RowMap -> GNUFoodData
parseGNUFoodData xs =
    GNUFoodData
        { calories = parseCalories xs,
          nutrients = parseNutrients xs
        }

readFloat :: T.Text -> Maybe Float
readFloat = readMaybe . T.unpack . T.strip

parseCalories :: RowMap -> Maybe U.EnergyDensity
parseCalories = fmap (U.kcal_per_100g .*.) . readFloat <=< M.lookup caloriesKey

parseNutrients :: RowMap -> M.Map T.Text U.MassFraction
parseNutrients xs = M.fromList $ mapMaybe (\k -> (k,) <$> getAmount k) nutrientsKeys
  where
    getUnit :: T.Text -> U.MassFraction
    getUnit key
        | "(g)" `T.isSuffixOf` key = U.g_per_100g
        | "(mg)" `T.isSuffixOf` key = U.mg_per_100g
        | "(mcg)" `T.isSuffixOf` key = U.ug_per_100g
        | "(mcg_RAE)" `T.isSuffixOf` key = U.ug_per_100g
        | "(mcg_DFE)" `T.isSuffixOf` key = U.ug_per_100g
        -- Should never happen, because all the keys are hardcoded.
        | otherwise = error $ "Unknown unit: " ++ T.unpack key
    parseAmount :: T.Text -> T.Text -> Maybe U.MassFraction
    parseAmount key val = (getUnit key .*.) <$> readFloat val
    getAmount :: T.Text -> Maybe U.MassFraction
    getAmount key = M.lookup key xs >>= parseAmount key

queryKeys :: T.Text -> [T.Text] -> S.Connection -> IO [RowMap]
queryKeys table keys conn = fmap toMap <$> S.query_ conn query
  where
    -- In an SQL query, column names with spaces in them have to be surrounded with double-quotes, e.g.:
    -- select "Food name","Protein (g)" from nutrients
    query = S.Query $ "select " <> T.intercalate "," (enquote <$> keys) <> " from " <> table
    enquote :: T.Text -> T.Text
    enquote x = "\"" <> x <> "\""
    toMap :: [T.Text] -> RowMap
    toMap = M.fromList . zip keys

loadGNU :: FilePath -> IO (M.Map GNUFoodID GNUFoodData)
loadGNU db = do
    conn <- S.open db
    (xs :: [RowMap]) <- queryKeys "nutrients" requiredCols conn
    S.close conn
    return $ M.fromList $ mapMaybe parseGNUFood xs
  where
    requiredCols :: [T.Text]
    requiredCols = nameKey : caloriesKey : nutrientsKeys
    parseGNUFood :: RowMap -> Maybe (GNUFoodID, GNUFoodData)
    parseGNUFood x = (,parseGNUFoodData x) <$> parseGNUFoodID x
