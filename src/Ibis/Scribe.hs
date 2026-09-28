-- | Module for compiling the food nutrient database, and serializing it.
module Ibis.Scribe (
    scribeUSDA,
) where

import Conduit (ConduitT, awaitForever, runConduitPure, sinkList, yield, (.|))
import qualified Crom.Food as F
import qualified Data.ByteString as B
import qualified Data.Conduit.Aeson as CA
import Data.Either (partitionEithers)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Ibis.Data.JSON (trimArray)
import qualified Ibis.Data.Zip as Z
import qualified Ibis.FoodDBs.USDA.Compile as C
import qualified Ibis.FoodDBs.USDA.Read as R

-- | Force evaluate each element inside Conduit's control flow.
forceC :: (Monad m) => ConduitT a a m ()
forceC = awaitForever $ \x -> x `seq` yield x

-- | Read a USDA JSON database into a list of RawFoods.
parseUSDA :: B.ByteString -> [Either CA.ParserError R.RawFood]
parseUSDA json =
    runConduitPure $
        yield (trimArray json)
            -- Parse directly to RawFood
            .| CA.conduitArrayEither
            -- Force evaluation to prevent memory leak
            .| forceC
            .| sinkList

-- | Create a nutrient database from the given USDA JSON food sources.
scribeUSDA :: [FilePath] -> FilePath -> IO ()
scribeUSDA srcs dst = do
    files <- concatMap Z.unzipAll <$> mapM B.readFile srcs
    let results = concatMap parseUSDA files :: [Either CA.ParserError R.RawFood]
        (parseErrs, rawFoods) = partitionEithers results :: ([CA.ParserError], [R.RawFood])
        foods = C.toFood <$> rawFoods :: [F.Food]
        partialErrs = rawFoods >>= R.listRawFoodErrs :: [T.Text]
    putStrLn "Parsing errors:"
    mapM_ print parseErrs
    putStrLn "Partial errors:"
    mapM_ TIO.putStrLn partialErrs
    B.writeFile dst (Z.zipContents "usdaHaskell.txt" (show foods))
