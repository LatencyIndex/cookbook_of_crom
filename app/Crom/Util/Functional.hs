module Crom.Util.Functional (
    mapFst, mapSnd
) where

mapFst :: (a -> x) -> (a, b) -> (x, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> x) -> (a, b) -> (a, x)
mapSnd f (a, b) = (a, f b)
