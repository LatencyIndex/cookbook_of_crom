module Ibis.JSON.Explore (
    JSType,
    fromJSON,
    fromJSONArray,
) where

import Conduit
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as T
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import Data.Conduit.Aeson
import Data.Conduit.List (catMaybes)
import Data.Either.Extra (eitherToMaybe)
import qualified Ibis.Data.MultiTrie as M
import Ibis.JSON (trimArray)

-- | Represents the type of a JSON value, ignoring its value.
-- Useful for exploring JSON structure.
data JSType
    = Nil
    | Bit
    | Num
    | Str
    | Arr
    | Key T.Key
    deriving (Eq, Ord)

instance Show JSType where
    show Nil = "Nil"
    show Bit = "Bit"
    show Num = "Num"
    show Str = "Str"
    show Arr = "Arr"
    show (Key k) = show k

fromJSON :: T.Value -> M.Forest JSType Int
fromJSON (T.String _) = M.leaf Str
fromJSON (T.Number _) = M.leaf Num
fromJSON (T.Bool _) = M.leaf Bit
fromJSON T.Null = M.leaf Nil
fromJSON (T.Array xs) = M.singleton Arr children
  where
    children :: M.Forest JSType Int
    children = foldMap fromJSON xs
fromJSON (T.Object xs) = mconcat $ uncurry M.singleton <$> children
  where
    children :: [(JSType, M.Forest JSType Int)]
    children = bimap Key fromJSON <$> KM.toList xs

-- | Collect the first n JSON values in the JSON array into a MultiTrie.
-- Uses Conduit to process the JSON one at a time,
-- so that large arrays can be processed without using too much memory.
fromJSONArray :: Maybe Int -> B.ByteString -> M.Forest JSType Int
fromJSONArray n json =
    runConduitPure $
        yield (trimArray json)
            -- Parse JSON array into values, one at a time
            .| conduitArrayEither
            -- Identity or take n values
            .| maybe (mapC id) takeC n
            -- Skip any JSON that fails decoding.
            .| mapC eitherToMaybe
            .| catMaybes
            -- Convert to JSType MultiTrie and monoidally collapse it.
            .| foldMapC fromJSON
