module Crom.Units.Duration (
    DurationUnit, Duration,
    in_s, as_s,
    in_min, as_min,
    in_h, as_h,
    in_days, as_days
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

-- Convenience conversion functions

in_s :: Duration -> Float
in_s = num . convert_to U_s
as_s :: Float -> Duration
as_s x = Qty x U_s

in_min :: Duration -> Float
in_min = num . convert_to U_min
as_min :: Float -> Duration
as_min x = Qty x U_min

in_h :: Duration -> Float
in_h = num . convert_to U_h
as_h :: Float -> Duration
as_h x = Qty x U_h

in_days :: Duration -> Float
in_days = num . convert_to U_day
as_days :: Float -> Duration
as_days x = Qty x U_day
