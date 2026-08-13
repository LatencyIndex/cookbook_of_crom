{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Crom.Util.JSON.NTrie (
    JNode,
    NTrie,
    fromJSON,
    fromJSONArray,
    size,
    printFreqs,
) where

import Conduit
import qualified Data.ByteString as B
import Data.Conduit.Aeson
import Data.Conduit.List (catMaybes)
import Data.Either.Extra (eitherToMaybe)
import Control.DeepSeq (NFData, force)
import GHC.Generics (Generic)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as T
import qualified Data.Map.Strict as M
import Crom.Util.JSON (trimArray)

data JNode
    = Nil
    | Bit
    | Num
    | Str
    | Arr
    | Key T.Key
    deriving (Eq, Ord, Generic, NFData)

instance Show JNode where
    show Nil = "Nil"
    show Bit = "Bit"
    show Num = "Num"
    show Str = "Str"
    show Arr = "Arr"
    show (Key k) = show k

-- Trie that counts the number of strings, i.e. a multi-trie.
-- A NTrie node is a set of keys, the counts of each key
-- (the number of strings that end with that key),
-- and the child trie of each key.
newtype NTrie k = NTrie (M.Map k (Int, NTrie k))
    deriving (Generic, NFData)

instance (Ord k, NFData k) => Semigroup (NTrie k) where
    -- Strict evaluation to prevent memory leak when doing large folds.
    (NTrie x) <> (NTrie y) = force $ NTrie $ M.unionWith f x y
      where
        f (i, lChild) (j, rChild) = (i + j, lChild <> rChild)

instance (Ord k, NFData k) => Monoid (NTrie k) where
    mempty = NTrie M.empty

singleton :: k -> NTrie k
singleton k = NTrie $ M.singleton k (1, NTrie M.empty)

size :: NTrie k -> Int
size (NTrie kvs) = M.size kvs + sum (size . snd <$> M.elems kvs)

fromJSON :: T.Value -> NTrie JNode
fromJSON (T.String _) = singleton Str
fromJSON (T.Number _) = singleton Num
fromJSON (T.Bool _) = singleton Bit
fromJSON T.Null = singleton Nil
fromJSON (T.Array xs) = NTrie $ M.singleton Arr (1, children)
  where
    children :: NTrie JNode
    children = foldMap fromJSON xs
fromJSON (T.Object xs) = NTrie $ M.fromList children
  where
    children :: [(JNode, (Int, NTrie JNode))]
    children = (\(k, v) -> (Key k, (1, fromJSON v))) <$> KM.toList xs

-- Collect the first n json values in the given json array into a NTrie.
-- Uses Conduit to process the json one at a time,
-- so that large arrays can be processed without using too much memory.
fromJSONArray :: Maybe Int -> B.ByteString -> NTrie JNode
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

-- Pretty-prints the trie, using relative counts - the probability a parent contains a given child.
printFreqs :: (Show k) => NTrie k -> IO ()
printFreqs = pprint 0 1
  where
    pprint :: (Show k) => Int -> Int -> NTrie k -> IO ()
    pprint indent parentCount (NTrie kvs) = mapM_ printKV $ M.toList kvs
      where
        prefix = replicate indent ' ' :: String
        printKV :: (Show k) => (k, (Int, NTrie k)) -> IO ()
        printKV (k, (childCount, v)) =
            let p = fromIntegral childCount / fromIntegral parentCount :: Float
             in do
                    putStrLn $ prefix ++ show k ++ " x " ++ show p
                    pprint (indent + 4) childCount v
