module Crom.IO.FoodDB (
    FoodDB,
    encode,
    load,
) where

import Crom.Food qualified as F
import Crom.IO.Zip qualified as Z
import Data.ByteString qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map.Strict qualified as M

type FoodDB = M.Map F.FoodID F.FoodData

-- Encode the database into a zip archive.
encode :: FoodDB -> B.ByteString
encode = Z.zipContents "usdaHaskell.txt" . show

-- Decode all the databases in a zip archive.
decodeDB :: B.ByteString -> FoodDB
decodeDB = M.unions . fmap readDB . Z.unzipAll
  where
    -- Read a single unzipped database.
    readDB :: B.ByteString -> FoodDB
    readDB = read . BL.unpack . BL.fromStrict

-- | Read a food database from a zip archive.
load :: FilePath -> IO FoodDB
load = fmap decodeDB . B.readFile
