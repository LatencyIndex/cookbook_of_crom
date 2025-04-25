module Crom.Units where

data Quantity u = Qty {num :: Float, unit :: u}
instance Show u => Show (Quantity u) where
    show (Qty num unit) = show num ++ show unit

-- Class for units of the same dimension, e.g. mass or length.
class Unit a where
    -- This can be defined directly, by implementing x `inUnitsOf` CanonicalUnit for every x
    -- (e.g. x `inUnitsOf` U_kg), and then implement conversion between every pair of units
    -- as the ratio of conversions to CanonicalUnit. However, because the ratio definition has
    -- a pattern of x `inUnitsOf` y, it will match every pattern, no warning is emitted if some
    -- conversions to CanonicalUnit are missing. So a toCanonical :: a -> Float
    -- helper function should be used, to get a warning if any cases are missing.
    inUnitsOf :: a -> a -> Float
    convertTo :: a -> Quantity a -> Quantity a
    convertTo w (Qty x u) = Qty ((u `inUnitsOf` w)*x) w

qMul :: Float -> Quantity a -> Quantity a
qMul k (Qty x u) = Qty (k*x) u

qDiv :: Unit a => Quantity a -> Quantity a -> Float
qDiv l r = num l / (num $ convertTo (unit l) r)

qSum :: Unit a => Quantity a -> Quantity a -> Quantity a
qSum l r = Qty (num l + (num $ convertTo u r)) u
    where u = unit l

qSub :: Unit a => Quantity a -> Quantity a -> Quantity a
qSub l r = Qty (num l - (num $ convertTo u r)) u
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
    x `inUnitsOf` y = in_g x / in_g y where
        in_g :: MassUnit -> Float
        in_g U_kg = 1e+3
        in_g U_g  = 1
        in_g U_mg = 1e-3
        in_g U_ug = 1e-6
        in_g U_lb = 453.59237

-- Convenience conversion functions

to_kg :: Mass -> Float
to_kg = num . convertTo U_kg
as_kg :: Float -> Mass
as_kg x = Qty x U_kg

to_g :: Mass -> Float
to_g = num . convertTo U_g
as_g :: Float -> Mass
as_g x = Qty x U_g

to_mg :: Mass -> Float
to_mg = num . convertTo U_mg
as_mg :: Float -> Mass
as_mg x = Qty x U_mg

to_ug :: Mass -> Float
to_ug = num . convertTo U_ug
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
    x `inUnitsOf` y = inKcal x / inKcal y where
        inKcal :: EnergyUnit -> Float
        inKcal U_kcal = 1
        inKcal U_kJ = 0.23900574
    convertTo w (Qty x u) = Qty ((u `inUnitsOf` w)*x) w

kcal :: Energy -> Float
kcal = num . convertTo U_kcal
asKcal :: Float -> Energy
asKcal x = Qty x U_kcal
