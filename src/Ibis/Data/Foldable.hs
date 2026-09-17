module Ibis.Data.Foldable (
    maximumOnMay,
) where

maxOn :: (Ord b) => (a -> b) -> a -> a -> a
maxOn f l r
    | f l >= f r = l
    | otherwise = r

maximumOnMay :: forall f a b. (Foldable f, Ord b) => (a -> b) -> f a -> Maybe a
maximumOnMay f = foldl' g Nothing
  where
    g :: Maybe a -> a -> Maybe a
    g acc x = maxOn (fmap f) acc (Just x)
