module Crom.Units (
    Quantity(..),
    Unit,
    qMul,
    qDiv,
    qSum,
    qSub,
    MassUnit,
    Mass,
    as_g,
    as_mg,
    as_ug,
    to_kg,
    EnergyUnit,
    Energy,
    as_kcal,
    DurationUnit,
    Duration
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


-- A MassUnit can be a kilogram, gram, the mass of a milliliter of milk, an egg, apple, a spoon of oil, etc.
data MassUnit
    = U_kg
    | U_g
    | U_mg
    | U_ug -- microgram
    | U_lb
instance Show MassUnit where
    show U_kg = "kg"
    show U_g  =  "g"
    show U_mg = "mg"
    show U_ug = "ug"
    show U_lb = "lb"
type Mass = Quantity MassUnit

instance Unit MassUnit where
    x `in_units_of` y = in_g x / in_g y where
        in_g :: MassUnit -> Float
        in_g U_kg = 1e+3
        in_g U_g  = 1
        in_g U_mg = 1e-3
        in_g U_ug = 1e-6
        in_g U_lb = 453.59237

-- Convenience conversion functions

to_kg :: Mass -> Float
to_kg = num . convert_to U_kg
as_kg :: Float -> Mass
as_kg x = Qty x U_kg

to_g :: Mass -> Float
to_g = num . convert_to U_g
as_g :: Float -> Mass
as_g x = Qty x U_g

to_mg :: Mass -> Float
to_mg = num . convert_to U_mg
as_mg :: Float -> Mass
as_mg x = Qty x U_mg

to_ug :: Mass -> Float
to_ug = num . convert_to U_ug
as_ug :: Float -> Mass
as_ug x = Qty x U_ug

data EnergyUnit
    = U_kcal
    | U_kJ
instance Show EnergyUnit where
    show U_kcal = "kcal"
    show U_kJ = "kJ"
type Energy = Quantity EnergyUnit
instance Unit EnergyUnit where
    x `in_units_of` y = in_kcal x / in_kcal y where
        in_kcal :: EnergyUnit -> Float
        in_kcal U_kcal = 1
        in_kcal U_kJ   = 0.23900574
    convert_to w (Qty x u) = Qty ((u `in_units_of` w)*x) w

kcal :: Energy -> Float
kcal = num . convert_to U_kcal
as_kcal :: Float -> Energy
as_kcal x = Qty x U_kcal

data DurationUnit
    = U_s
    | U_min
    | U_h
    | U_day
instance Show DurationUnit where
    show U_s   = "s"
    show U_min = "min"
    show U_h   = "h"
    show U_day = "days"
type Duration = Quantity DurationUnit
instance Unit DurationUnit where
    x `in_units_of` y = in_s x / in_s y where
        in_s :: DurationUnit -> Float
        in_s U_s   = 1
        in_s U_min = 60 * in_s U_s
        in_s U_h   = 60 * in_s U_min
        in_s U_day = 24 * in_s U_h
