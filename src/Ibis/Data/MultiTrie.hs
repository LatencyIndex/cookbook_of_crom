module Ibis.Data.MultiTrie (
    MultiTrie (..),
    Forest (..),
    singleton,
    leaf,
    fromList,
    toProbability,
    toProbabilities,
) where

import qualified Data.Map.Strict as M

-- | If stored under a key in a Forest, count is the number of times that key was inserted.
data MultiTrie k n = MultiTrie {count :: n, subForest :: Forest k n}

newtype Forest k n = Forest {unwrap :: M.Map k (MultiTrie k n)}

instance forall k n. (Ord k, Num n) => Semigroup (Forest k n) where
    (Forest l) <> (Forest r) = Forest $ M.unionWith f l r
      where
        f :: MultiTrie k n -> MultiTrie k n -> MultiTrie k n
        -- Strict evaluation to prevent memory leak during folds.
        f x y = n `seq` forest `seq` MultiTrie n forest
          where
            n = x.count + y.count
            forest = x.subForest <> y.subForest

instance (Ord k, Num n) => Monoid (Forest k n) where
    mempty = Forest M.empty

instance forall k n. (Show k, Show n) => Show (Forest k n) where
    show = unlines . pshow 0
      where
        pshow :: Int -> Forest k n -> [String]
        pshow depth (Forest forest) = foldMap (showkv depth) (M.toList forest)
        indent :: Int -> String
        indent depth = replicate (depth * 4) ' '
        showkv :: Int -> (k, MultiTrie k n) -> [String]
        showkv depth (k, (MultiTrie n forest)) = root : subforest
          where
            root = indent depth ++ show k ++ " x " ++ show n :: String
            subforest = pshow (depth + 1) forest :: [String]

singleton :: (Ord k, Num n) => k -> Forest k n -> Forest k n
singleton k forest = Forest $ M.singleton k (MultiTrie 1 forest)

leaf :: (Ord k, Num n) => k -> Forest k n
leaf k = singleton k mempty

fromList :: (Ord k, Num n) => [k] -> Forest k n
fromList = mconcat . fmap leaf

-- | Convert counts to the ratio of count / parentCount,
-- i.e. the probability that a node, if it exists, has that child.
toProbability :: (Integral n) => MultiTrie k n -> MultiTrie k Float
toProbability = f 1
  where
    f :: (Integral n) => n -> MultiTrie k n -> MultiTrie k Float
    f nParent (MultiTrie n forest) = MultiTrie (n' / nParent') (Forest $ f n <$> forest.unwrap)
      where
        nParent' = fromIntegral nParent
        n' = fromIntegral n

-- | Apply toProbability to the whole forest.
toProbabilities :: (Integral n) => Forest k n -> Forest k Float
toProbabilities = Forest . fmap toProbability . (.unwrap)
