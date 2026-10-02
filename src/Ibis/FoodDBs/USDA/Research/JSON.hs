{-# LANGUAGE TypeApplications #-}

-- | Utilities to help investigate JSON food data sources.
-- E.g. print the shape of JSON data, or pretty-print example JSON entries.
module Ibis.FoodDBs.USDA.Research.JSON (
    prettyPrintArray,
    printProbabilityTrie,
    allNutrientNames,
    allUnitNames,
) where

import Conduit (foldC, mapC, mapM_C, runConduit, runConduitPure, takeC, yield, (.|))
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty qualified as P
import Data.Aeson.Types qualified as J
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Conduit.Aeson (conduitArrayEither)
import Data.Conduit.List (catMaybes)
import Data.Either.Extra (eitherToMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Ibis.Data.JSON (trimArray)
import Ibis.Data.JSON.JTrie qualified as JT
import Ibis.Data.MultiTrie qualified as MT
import Ibis.FoodDBs.USDA.Read qualified as R

-- | Pretty-print the first n JSON values from a JSON array.
-- Uses Conduit to parse the values one at a time,
-- so it uses little memory even for very large arrays.
prettyPrintArray :: Maybe Int -> B.ByteString -> IO ()
prettyPrintArray n json =
    runConduit $
        yield (trimArray json)
            .| conduitArrayEither @Value
            -- Identity or take n values
            .| maybe (mapC id) takeC n
            -- Skip any json that fails decoding.
            .| mapC eitherToMaybe
            .| catMaybes
            .| mapM_C (BL.putStr . P.encodePretty)

-- | Convert the first n JSON values to a probability trie and print it,
-- showing the kinds of values and their likelihoods, given their parent exists.
printProbabilityTrie :: Maybe Int -> B.ByteString -> IO ()
printProbabilityTrie n json = print $ MT.toProbabilities $ JT.fromJSONArray n json

-- | Parse JSON into RawFood, extract data, and monoidally combine it.
gatherRawField :: (Monoid m) => (R.JSFood -> m) -> B.ByteString -> m
gatherRawField f json =
    runConduitPure $
        yield (trimArray json)
            -- Parse string to JSON
            .| conduitArrayEither @Value
            -- Skip any JSON that failed parsing
            .| mapC eitherToMaybe
            .| catMaybes
            -- Parse JSON to RawFood
            .| mapC (J.parseMaybe (J.parseJSON @R.JSFood))
            -- Skip failed parses
            .| catMaybes
            -- Extract the monoid value
            .| mapC f
            -- Combine the monoid values
            .| foldC

-- | All distinct nutrient names from the JSON dataset.
allNutrientNames :: B.ByteString -> S.Set T.Text
allNutrientNames = gatherRawField (S.fromList . fmap (.nutrient.name) . (.foodNutrients))

-- | All distinct unit names from the JSON dataset.
allUnitNames :: B.ByteString -> S.Set T.Text
allUnitNames = gatherRawField (S.fromList . fmap (.nutrient.unitName) . (.foodNutrients))
