module Ibis.Data.MultiMap (
    MultiMap,
    fromList,
    insert,
) where

import Data.List.NonEmpty qualified as N
import Data.Map.Strict qualified as M

type MultiMap k a = M.Map k (N.NonEmpty a)

fromList :: (Ord k) => [(k, a)] -> MultiMap k a
fromList = M.fromListWith (<>) . fmap (fmap N.singleton)

insert :: (Ord k) => k -> a -> MultiMap k a -> MultiMap k a
insert k v = M.insertWith N.append k (N.singleton v)
