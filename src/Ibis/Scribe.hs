-- | Module for compiling the food nutrient database, and serializing it.
module Ibis.Scribe (
    scribeUSDA,
) where

import Conduit (foldlC, runConduitPure, yield, (.|))
import Crom.Food qualified as F
import Crom.IO.Zip.Read qualified as ZR
import Data.ByteString qualified as B
import Data.Conduit.Aeson qualified as CA
import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as N
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Ibis.Data.JSON (trimArray)
import Ibis.Data.MultiMap qualified as MM
import Ibis.FoodDBs.USDA.Compile qualified as C
import Ibis.FoodDBs.USDA.Read qualified as R
import Ibis.IO.Zip.Write qualified as ZW

-- Force evaluation to prevent memory leak
forceCons :: [a] -> a -> [a]
forceCons acc x = x `seq` x : acc

-- | Read a USDA JSON database into a list of RawFoods.
parseUSDA :: B.ByteString -> [Either CA.ParserError R.RawFood]
parseUSDA json =
    runConduitPure $
        yield (trimArray json)
            -- Parse directly to RawFood
            .| CA.conduitArrayEither
            -- Conduit's sinkList is too lazy, and causes a memory leak
            .| foldlC forceCons []

-- | Create a nutrient database from the given USDA JSON food sources.
scribeUSDA :: [FilePath] -> FilePath -> IO ()
scribeUSDA srcs dst = do
    files <- concatMap ZR.unzipAll <$> mapM B.readFile srcs
    let results = concatMap parseUSDA files :: [Either CA.ParserError R.RawFood]
        (parseErrs, rawFoods) = partitionEithers results :: ([CA.ParserError], [R.RawFood])
        foods = C.toFood <$> rawFoods :: [(F.FoodID, F.FoodData)]
        partialErrs = filter (not . T.null) $ R.listRawFoodErrs <$> rawFoods :: [T.Text]
    putStrLn "Parsing errors:"
    mapM_ print parseErrs
    putStrLn "Partial errors:"
    mapM_ TIO.putStrLn partialErrs
    putStrLn "Duplicates:"
    let foodMap = MM.fromList foods
        dups = M.toList $ M.filter ((> 1) . N.length) foodMap
    mapM_ (\(name, dfoods) -> TIO.putStrLn (T.show name <> ": " <> T.show (N.length dfoods))) dups
    B.writeFile dst (ZW.zipContents "usdaHaskell.txt" (show foods))
