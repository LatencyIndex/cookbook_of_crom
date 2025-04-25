{-# LANGUAGE ScopedTypeVariables #-}

module Crom.Units.Units (
    Quantity(..),
    Unit(..),
    (.*),
    (./),
    (.+),
    (.-)
) where

data Quantity u = Qty {num :: Float, unit :: u}
instance Show u => Show (Quantity u) where
    show (Qty num unit) = show num ++ show unit

-- Class for units of the same dimension, e.g. mass or length.
class Unit a where
    canonical :: a -- The default/preferred unit. E.g. g for mass (and not kg or mg)
    -- If canonical = g, then in_canonical mg = 0.001, in_canonical g = 1, and in_canonical kg = 1000
    in_canonical :: a -> Float
    convert_to :: a -> Quantity a -> Quantity a
    convert_to w (Qty x u) = Qty (x * u `in_units_of` w) w where
        -- kg `in_units_of` g = 1000
        in_units_of :: a -> a -> Float
        u `in_units_of` w = in_canonical u / in_canonical w
instance Unit a => Semigroup (Quantity a) where
    (<>) = (.+)
instance Unit a => Monoid (Quantity a) where
    mempty = Qty 0 canonical

(.*) :: Float -> Quantity a -> Quantity a
k .* (Qty x u) = Qty (k*x) u

(./) :: Unit a => Quantity a -> Quantity a -> Float
l ./ r = num l / num (convert_to (unit l) r)

(.+) :: Unit a => Quantity a -> Quantity a -> Quantity a
l .+ r = Qty (num l + num (convert_to u r)) u
    where u = unit l

(.-) :: Unit a => Quantity a -> Quantity a -> Quantity a
l .- r = Qty (num l - num (convert_to u r)) u
    where u = unit l
