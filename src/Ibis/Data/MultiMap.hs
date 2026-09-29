module Ibis.Data.MultiMap (
    MultiMap,
    fromList,
) where

import Data.List.NonEmpty qualified as N
import Data.Map.Strict qualified as M

type MultiMap k a = M.Map k (N.NonEmpty a)

fromList :: (Ord k) => [(k, a)] -> MultiMap k a
fromList = M.fromListWith (<>) . fmap (fmap N.singleton)
