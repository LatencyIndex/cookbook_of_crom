module Crom.Units.Units (
    Quantity(..),
    Unit(..),
    qMul,
    qDiv,
    qSum,
    qSub
) where

data Quantity u = Qty {num :: Float, unit :: u}
instance Show u => Show (Quantity u) where
    show (Qty num unit) = show num ++ show unit

-- Class for units of the same dimension, e.g. mass or length.
class Unit a where
    -- This can be defined directly, by implementing x `in_units_of` CanonicalUnit for every x
    -- (e.g. x `in_units_of` U_kg), and then implement conversion between every pair of units
    -- as the ratio of conversions to CanonicalUnit. However, because the ratio definition has
    -- a pattern of x `in_units_of` y, it will match every pattern, so no warning is emitted if
    -- some conversions to CanonicalUnit are missing. So in_units_of should be implemented through
    -- a toCanonical :: a -> Float helper function, to get a warning if any cases are missing.
    in_units_of :: a -> a -> Float
    convert_to :: a -> Quantity a -> Quantity a
    convert_to w (Qty x u) = Qty ((u `in_units_of` w)*x) w

qMul :: Float -> Quantity a -> Quantity a
qMul k (Qty x u) = Qty (k*x) u

qDiv :: Unit a => Quantity a -> Quantity a -> Float
qDiv l r = num l / (num $ convert_to (unit l) r)

qSum :: Unit a => Quantity a -> Quantity a -> Quantity a
qSum l r = Qty (num l + (num $ convert_to u r)) u
    where u = unit l

qSub :: Unit a => Quantity a -> Quantity a -> Quantity a
qSub l r = Qty (num l - (num $ convert_to u r)) u
    where u = unit l
