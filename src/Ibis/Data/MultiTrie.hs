module Ibis.Data.MultiTrie (
    MultiTrie (..),
    singleton,
    size,
    printFreqs,
) where

import qualified Data.Map.Strict as M

-- Strict version of (,)
pair' :: a -> b -> (a, b)
pair' a b = a `seq` b `seq` (a, b)

-- Trie that counts the number of strings, i.e. a multi-trie.
-- A MultiTrie node is a set of keys, the counts of each key
-- (the number of strings that end with that key),
-- and the child trie of each key.
newtype MultiTrie k = MultiTrie (M.Map k (Int, MultiTrie k))

instance (Ord k) => Semigroup (MultiTrie k) where
    (MultiTrie x) <> (MultiTrie y) = MultiTrie $ M.unionWith f x y
      where
        -- Strict evaluation to prevent memory leak during large folds.
        f (i, lChild) (j, rChild) = pair' (i + j) (lChild <> rChild)

instance (Ord k) => Monoid (MultiTrie k) where
    mempty = MultiTrie M.empty

singleton :: k -> MultiTrie k
singleton k = MultiTrie $ M.singleton k (1, MultiTrie M.empty)

size :: MultiTrie k -> Int
size (MultiTrie kvs) = M.size kvs + sum (size . snd <$> M.elems kvs)

-- Pretty-prints the trie, using relative counts - the probability a parent contains a given child.
printFreqs :: (Show k) => MultiTrie k -> IO ()
printFreqs = pprint 0 1
  where
    pprint :: (Show k) => Int -> Int -> MultiTrie k -> IO ()
    pprint indent parentCount (MultiTrie kvs) = mapM_ printKV $ M.toList kvs
      where
        prefix = replicate indent ' ' :: String
        printKV :: (Show k) => (k, (Int, MultiTrie k)) -> IO ()
        printKV (k, (childCount, v)) =
            let p = fromIntegral childCount / fromIntegral parentCount :: Float
             in do
                    putStrLn $ prefix ++ show k ++ " x " ++ show p
                    pprint (indent + 4) childCount v
