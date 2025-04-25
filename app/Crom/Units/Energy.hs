module Crom.Units.Energy (
    EnergyUnit, Energy,
    in_kcal, as_kcal,
    in_kJ, as_kJ
) where

import Crom.Units.Units

data EnergyUnit
    = U_kcal
    | U_kJ
instance Show EnergyUnit where
    show U_kcal = "kcal"
    show U_kJ = "kJ"
type Energy = Quantity EnergyUnit
instance Unit EnergyUnit where
    canonical = U_kcal
    in_canonical U_kcal = 1
    in_canonical U_kJ   = 0.23900574

-- Convenience conversion functions

in_kcal :: Energy -> Float
in_kcal = num . convert_to U_kcal
as_kcal :: Float -> Energy
as_kcal x = Qty x U_kcal

in_kJ :: Energy -> Float
in_kJ = num . convert_to U_kJ
as_kJ :: Float -> Energy
as_kJ x = Qty x U_kJ
