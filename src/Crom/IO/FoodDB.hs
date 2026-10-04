module Crom.IO.FoodDB (
    FoodDB,
    encode,
    load,
) where

import Crom.Food qualified as F
import Crom.IO.Zip qualified as Z
import Data.Binary qualified as BI
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map.Strict qualified as M

type FoodDB = M.Map F.FoodID F.FoodData

-- Encode the database into a zip archive.
encode :: FoodDB -> BL.ByteString
encode = Z.zipContents "usdaHaskell.bin" . BI.encode

-- Decode all the databases in a zip archive.
decodeDB :: BL.ByteString -> FoodDB
decodeDB = M.unions . fmap BI.decode . Z.unzipAll

-- | Read a food database from a zip archive.
load :: FilePath -> IO FoodDB
load = fmap decodeDB . BL.readFile
