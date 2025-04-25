module Crom.Units.Mass (
    MassUnit, Mass,
    in_kg, as_kg,
    in_g, as_g,
    in_mg, as_mg,
    in_ug, as_ug
) where

import Crom.Units.Units

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
    canonical = U_g
    in_canonical U_kg = 1e+3
    in_canonical U_g  = 1
    in_canonical U_mg = 1e-3
    in_canonical U_ug = 1e-6
    in_canonical U_lb = 453.59237

-- Convenience conversion functions

in_kg :: Mass -> Float
in_kg = num . convert_to U_kg
as_kg :: Float -> Mass
as_kg x = Qty x U_kg

in_g :: Mass -> Float
in_g = num . convert_to U_g
as_g :: Float -> Mass
as_g x = Qty x U_g

in_mg :: Mass -> Float
in_mg = num . convert_to U_mg
as_mg :: Float -> Mass
as_mg x = Qty x U_mg

in_ug :: Mass -> Float
in_ug = num . convert_to U_ug
as_ug :: Float -> Mass
as_ug x = Qty x U_ug
