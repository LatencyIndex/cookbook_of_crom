module Crom.Units.Mass (
    MassUnit,
    Mass,
    as_g,
    as_mg,
    as_ug,
    to_kg
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
