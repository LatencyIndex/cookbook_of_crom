{-# LANGUAGE TypeApplications #-}

-- | Module for compiling the food nutrient database, and serializing it.
module Ibis.Scribe (
    scribeUSDA,
) where

import Conduit (ConduitT, awaitForever, mapC, runConduitPure, sinkList, yield, (.|))
import qualified Crom.Food as F
import qualified Data.Aeson.Types as J
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.Conduit.Aeson as CA
import Data.Either (partitionEithers)
import Ibis.Data.JSON (trimArray)
import qualified Ibis.Data.Zip as Z
import qualified Ibis.FoodDBs.USDA.Compile as C

-- | Force evaluate each element inside Conduit's control flow.
forceC :: (Monad m) => ConduitT a a m ()
forceC = awaitForever $ \x -> x `seq` yield x

-- | Read a USDA JSON database into a list of Foods.
parseUSDA :: B.ByteString -> [Either String F.Food]
parseUSDA json =
    runConduitPure $
        yield (trimArray json)
            -- Parse string to JSON
            .| CA.conduitArrayEither @J.Value
            -- Convert error type to String to match subsequent errors
            .| mapC (first show)
            -- Convert to RawFood
            .| mapC (>>= J.parseEither J.parseJSON)
            -- Convert to Food
            .| mapC (fmap C.toFood)
            -- Force evaluation to prevent memory leak
            .| forceC
            .| sinkList

-- | Create a nutrient database from the given USDA JSON food sources.
scribeUSDA :: [FilePath] -> FilePath -> IO ()
scribeUSDA srcs dst = do
    files <- concatMap Z.unzipAll <$> mapM B.readFile srcs
    let results = concatMap parseUSDA files :: [Either String F.Food]
        (errors, values) = partitionEithers results :: ([String], [F.Food])
    mapM_ putStrLn errors
    B.writeFile dst (Z.zipContents "usdaHaskell.txt" (show values))
