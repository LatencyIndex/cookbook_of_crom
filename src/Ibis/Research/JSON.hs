{-# LANGUAGE TypeApplications #-}

module Ibis.Research.JSON (
    prettyPrintArray,
) where

import Conduit (mapC, mapM_C, runConduit, takeC, yield, (.|))
import Data.Aeson (Value)
import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit.Aeson (conduitArrayEither)
import Data.Conduit.List (catMaybes)
import Data.Either.Extra (eitherToMaybe)
import Ibis.Data.JSON (trimArray)

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
