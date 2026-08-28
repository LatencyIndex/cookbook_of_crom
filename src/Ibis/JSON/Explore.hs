module Ibis.JSON.Explore (
    JNode,
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
import Ibis.Data.MultiTrie
import Ibis.JSON (trimArray)

data JNode
    = Nil
    | Bit
    | Num
    | Str
    | Arr
    | Key T.Key
    deriving (Eq, Ord)

instance Show JNode where
    show Nil = "Nil"
    show Bit = "Bit"
    show Num = "Num"
    show Str = "Str"
    show Arr = "Arr"
    show (Key k) = show k

fromJSON :: T.Value -> MultiTrie JNode
fromJSON (T.String _) = singleton Str
fromJSON (T.Number _) = singleton Num
fromJSON (T.Bool _) = singleton Bit
fromJSON T.Null = singleton Nil
fromJSON (T.Array xs) = fromList [(Arr, children)]
  where
    children :: MultiTrie JNode
    children = foldMap fromJSON xs
fromJSON (T.Object xs) = fromList children
  where
    children :: [(JNode, MultiTrie JNode)]
    children = bimap Key fromJSON <$> KM.toList xs

-- Collect the first n json values in the given json array into a MultiTrie.
-- Uses Conduit to process the json one at a time,
-- so that large arrays can be processed without using too much memory.
fromJSONArray :: Maybe Int -> B.ByteString -> MultiTrie JNode
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
            .| foldMapC fromJSON
