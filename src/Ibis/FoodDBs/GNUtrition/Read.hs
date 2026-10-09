module Ibis.FoodDBs.GNUtrition.Read (
    loadGNU,
) where

import Control.Monad ((<=<))
import Crom.Units ((.*))
import Crom.Units qualified as U
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Database.SQLite.Simple qualified as S
import Ibis.FoodDBs.GNUtrition.DBColumns as Cols
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

data GNUFoodID = GNUFoodID
    { name :: T.Text,
      code :: Maybe Int
    }
    deriving (Eq, Ord)

data GNUFoodData = GNUFoodData
    { calories :: Maybe U.EnergyDensity,
      nutrients :: M.Map T.Text U.MassFraction
    }
    deriving (Show)

-- | Strip text of whitespace, then attempt to read it.
readStripped :: (Read a) => T.Text -> Maybe a
readStripped = readMaybe . T.unpack . T.strip

parseGNUFoodID :: RowMap -> GNUFoodID
parseGNUFoodID xs =
    GNUFoodID
        { name = fromMaybe "" (M.lookup Cols.name xs),
          code = M.lookup Cols.foodCode xs >>= readStripped
        }

parseGNUFoodData :: RowMap -> GNUFoodData
parseGNUFoodData xs =
    GNUFoodData
        { calories = parseCalories xs,
          nutrients = parseNutrients xs
        }

parseCalories :: RowMap -> Maybe U.EnergyDensity
parseCalories = fmap (U.kcal_per_100g .*) . readStripped <=< M.lookup Cols.calories

parseNutrients :: RowMap -> M.Map T.Text U.MassFraction
parseNutrients xs = M.fromList $ mapMaybe (\k -> (k,) <$> getAmount k) Cols.nutrients
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
    parseAmount key val = (getUnit key .*) <$> readStripped val
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
    return $ M.fromList $ parseGNUFood <$> xs
  where
    requiredCols :: [T.Text]
    requiredCols = Cols.name : Cols.foodCode : Cols.calories : Cols.nutrients
    parseGNUFood :: RowMap -> (GNUFoodID, GNUFoodData)
    parseGNUFood x = (parseGNUFoodID x, parseGNUFoodData x)
