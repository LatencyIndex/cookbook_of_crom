module Ibis.Data.NTrie (
    NTrie (..),
    singleton,
    size,
    printFreqs,
) where

import qualified Data.Map.Strict as M

-- Strict version of (,)
pair' :: a -> b -> (a, b)
pair' a b = a `seq` b `seq` (a, b)

-- Trie that counts the number of strings, i.e. a multi-trie.
-- A NTrie node is a set of keys, the counts of each key
-- (the number of strings that end with that key),
-- and the child trie of each key.
newtype NTrie k = NTrie (M.Map k (Int, NTrie k))

instance (Ord k) => Semigroup (NTrie k) where
    (NTrie x) <> (NTrie y) = NTrie $ M.unionWith f x y
      where
        -- Strict evaluation to prevent memory leak during large folds.
        f (i, lChild) (j, rChild) = pair' (i + j) (lChild <> rChild)

instance (Ord k) => Monoid (NTrie k) where
    mempty = NTrie M.empty

singleton :: k -> NTrie k
singleton k = NTrie $ M.singleton k (1, NTrie M.empty)

size :: NTrie k -> Int
size (NTrie kvs) = M.size kvs + sum (size . snd <$> M.elems kvs)

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
