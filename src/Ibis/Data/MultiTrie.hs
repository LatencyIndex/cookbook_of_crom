module Ibis.Data.MultiTrie (
    Node (..),
    MultiTrie (..),
    fromList,
    singleton,
    size,
    printFreqs,
) where

import qualified Data.Map.Strict as M

data Node k = Node
    { -- | Number of trie keys that terminate in this node
      count :: Int,
      children :: MultiTrie k
    }

instance (Ord k) => Semigroup (Node k) where
    -- Strict evaluation to prevent memory leak during large folds.
    (Node i lChild) <> (Node j rChild) = count `seq` children `seq` Node count children
      where
        count = i + j
        children = lChild <> rChild

-- | Trie that counts the number of strings.
newtype MultiTrie k = MultiTrie (M.Map k (Node k))

instance (Ord k) => Semigroup (MultiTrie k) where
    (MultiTrie x) <> (MultiTrie y) = MultiTrie $ M.unionWith (<>) x y

instance (Ord k) => Monoid (MultiTrie k) where
    mempty = MultiTrie M.empty

-- | Build MultiTrie from root-level nodes.
fromList :: (Ord k) => [(k, MultiTrie k)] -> MultiTrie k
fromList = MultiTrie . M.fromList . fmap (Node 1 <$>)

-- | MultiTrie containing a single key.
singleton :: (Ord k) => k -> MultiTrie k
singleton k = fromList [(k, mempty)]

-- | Number of nodes in the trie.
size :: MultiTrie k -> Int
size (MultiTrie kvs) = M.size kvs + sum (size . (.children) <$> M.elems kvs)

-- | Pretty-print the trie, using relative counts - the probability a parent contains a given child.
printFreqs :: (Show k) => MultiTrie k -> IO ()
printFreqs = pprint 0 1
  where
    pprint :: (Show k) => Int -> Int -> MultiTrie k -> IO ()
    pprint indent parentCount (MultiTrie kvs) = mapM_ printKV $ M.toList kvs
      where
        prefix = replicate indent ' ' :: String
        printKV :: (Show k) => (k, Node k) -> IO ()
        printKV (k, node) =
            let p = fromIntegral node.count / fromIntegral parentCount :: Float
             in do
                    putStrLn $ prefix ++ show k ++ " x " ++ show p
                    pprint (indent + 4) node.count node.children
