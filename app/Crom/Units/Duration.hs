module Crom.Units.Duration (
    DurationUnit,
    Duration
) where

import Crom.Units.Units

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
