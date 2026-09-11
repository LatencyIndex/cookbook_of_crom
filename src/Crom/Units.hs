{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

-- A simple implementation of compile-time physical units.
module Crom.Units (
    Mass, Energy, Time,
    MassFraction, EnergyDensity,
    QMul, QDiv,
    (.*.), (./.),
    (.+.), (.-.), (*.), (.*), (./),
    toMFrac, mFracDiv,
    ug, mg, g, kg, lb,
    kJ, kcal,
    day,
    g_per_kg, ug_per_100g, mg_per_100g, g_per_100g,
    kcal_per_kg, kcal_per_100g, kJ_per_100g,
) where

import Data.Function (on)
import Crom.Util.Text (showSigDigits)

-- A quantity of dimension a, its value stored in some canonical unit.
newtype Qty a = Qty {val :: Float}

-- Dimensions
data DMass
data DEnergy
data DTime
data DMassFraction
data DEnergyDensity
-- Quantities
type Mass = Qty DMass
type Energy = Qty DEnergy
type Time = Qty DTime
type MassFraction = Qty DMassFraction
type EnergyDensity = Qty DEnergyDensity

(.+.) :: Qty a -> Qty a -> Qty a
(Qty x) .+. (Qty y) = Qty (x+y)
(.-.) :: Qty a -> Qty a -> Qty a
(Qty x) .-. (Qty y) = Qty (x-y)

instance Eq        (Qty a) where (==) = (==) `on` (.val)
instance Ord       (Qty a) where compare = compare `on` (.val)
instance Semigroup (Qty a) where (<>) = (.+.)
instance Monoid    (Qty a) where mempty = Qty 0

class QMul a b c | a b -> c where (.*.) :: a -> b -> c
class QDiv a b c | a b -> c where (./.) :: a -> b -> c

-- Internal functions for more convenient implementation
qmul :: Qty a -> Qty b -> Qty c
qmul (Qty x) (Qty y) = Qty (x*y)
qdiv :: Qty a -> Qty b -> Qty c
qdiv (Qty x) (Qty y) = Qty (x/y)

-- Anything more generic or clever runs into problems with conflicting instances.
instance QMul Float         (Qty a)       (Qty a)       where x .*. (Qty y) = Qty (x*y)
instance QMul (Qty a)       Float         (Qty a)       where (Qty x) .*. y = Qty (x*y)
instance QMul MassFraction  Mass          Mass          where (.*.) = qmul
instance QMul Mass          MassFraction  Mass          where (.*.) = qmul
instance QMul MassFraction  MassFraction  MassFraction  where (.*.) = qmul
instance QMul EnergyDensity Mass          Energy        where (.*.) = qmul
instance QMul Mass          EnergyDensity Energy        where (.*.) = qmul
instance QMul MassFraction  EnergyDensity EnergyDensity where (.*.) = qmul
instance QMul EnergyDensity MassFraction  EnergyDensity where (.*.) = qmul

instance QDiv (Qty a)       Float         (Qty a)       where (Qty x) ./. y = Qty (x/y)
instance QDiv (Qty a)       (Qty a)       Float         where (Qty x) ./. (Qty y) = x/y
instance QDiv Energy        Mass          EnergyDensity where (./.) = qdiv

-- Numeric literals have a type Num x => x. For some reason, this prevents Haskell
-- from determining which QMul or QDiv instance to use, if a numeric literal is the
-- 1st or 2nd argument to (.*.) or (./.), unless the literal is explicitly marked as Float.
-- So we define versions of (.*.) and (./.) that ergonomically handle numeric literals.
-- Mnemonic: the dot in the operator is on the same side as the Qty.
(*.) :: Float -> Qty a -> Qty a
(*.) = (.*.)
(.*) :: Qty a -> Float -> Qty a
(.*) = (.*.)
(./) :: Qty a -> Float -> Qty a
(./) = (./.)

-- MassFraction has meaning beyond its dimensionless units.
toMFrac :: Float -> MassFraction
toMFrac = Qty
mFracDiv :: Mass -> Mass -> MassFraction
x `mFracDiv` y = toMFrac (x ./. y)

-- Mass units
ug, mg, g, kg, lb :: Mass
ug = 1e-6 *. g
mg = 1e-3 *. g
g = Qty 1
kg = 1e+3 *. g
lb = 453.59237 *. g
-- Energy units
kcal, kJ :: Energy
kcal = Qty 1
kJ = 0.23900574 *. kcal
-- Time units
day :: Time
day = Qty 1
-- MassFraction units
g_per_kg, ug_per_100g, mg_per_100g, g_per_100g :: MassFraction
g_per_kg = g `mFracDiv` kg
ug_per_100g = ug `mFracDiv` (100 *. g)
mg_per_100g = mg `mFracDiv` (100 *. g)
g_per_100g = g `mFracDiv` (100 *. g)
-- EnergyDensity units
kcal_per_kg, kcal_per_100g, kJ_per_100g :: EnergyDensity
kcal_per_kg = kcal ./. kg
kcal_per_100g = kcal ./. (100 *. g)
kJ_per_100g = kJ ./. (100 *. g)

-- Shows the quantity and unit, to 3 significant digits.
-- 'units' must be non-empty.
qShow :: [(Qty a, String)] -> Qty a -> String
qShow units q = showSigDigits 3 (q ./. uval) ++ " " ++ uname where
    smallest = minimum units
    smaller = filter ((<= q) . fst) units
    -- Largest unit no larger than q, or the smallest unit if q is smaller than all others:
    largest = maximum (smallest : smaller)
    (uval, uname) = largest

instance Show Mass          where show   = qShow [(ug, "µg"), (mg, "mg"), (g, "g"), (kg, "kg")]
instance Show Energy        where show   = qShow [(kcal, "kcal")]
instance Show Time          where show   = qShow [(day, "days")]
instance Show MassFraction  where show x = show (x .* 100 .*. g) ++ " / 100 g"
instance Show EnergyDensity where show x = show (x .* 100 .*. g) ++ " / 100 g"
