{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

-- A simple implementation of compile-time physical units.
module Crom.Units (
    Mass, Energy, Time,
    MassFraction, EnergyDensity,
    QSum, QMul, QDiv,
    (.*.), (./.),
    (.+.), (.-.), (*.), (.*), (./),
    zeroQty, sumQty, avgQty,
    toMFrac, mFracDiv,
    ug, mg, g, kg, lb,
    kJ, kcal,
    day,
    g_per_kg, ug_per_100g, mg_per_100g, g_per_100g,
    kcal_per_kg, kcal_per_100g, kJ_per_100g,
) where

import qualified Crom.Util.Text as T
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as N
import qualified Data.List.NonEmpty.Extra as NE

-- A quantity of dimension a, its value stored in some canonical unit.
newtype Qty a = Qty {val :: Float} deriving (Show, Read, Eq, Ord)

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

class QSum a where
    (.+.) :: a -> a -> a
    zeroQty :: a
    sumQty :: (Foldable f) => f a -> a
    sumQty = foldr (.+.) zeroQty
    -- | Nothing if foldable is empty.
    avgQty :: (Foldable f, QSum a, QDiv a Float a) => f a -> Maybe a
    avgQty xs
        | n > 0 = Just $ sumQty xs ./. (fromIntegral n :: Float)
        | otherwise = Nothing
        where n = length xs
class QSub a where
    (.-.) :: a -> a -> a
class QMul a b c | a b -> c where (.*.) :: a -> b -> c
class QDiv a b c | a b -> c where (./.) :: a -> b -> c

instance QSum Float where
    (.+.) = (+)
    zeroQty = 0
instance QSum Int where
    (.+.) = (+)
    zeroQty = 0
instance QSum (Qty a) where
    (Qty x) .+. (Qty y) = Qty (x+y)
    zeroQty = Qty 0

instance QSub Float where
    (.-.) = (-)
instance QSub Int where
    (.-.) = (-)
instance QSub (Qty a) where
    (Qty x) .-. (Qty y) = Qty (x-y)

-- Internal functions for more convenient implementation
qmul :: Qty a -> Qty b -> Qty c
qmul (Qty x) (Qty y) = Qty (x*y)
qdiv :: Qty a -> Qty b -> Qty c
qdiv (Qty x) (Qty y) = Qty (x/y)

-- Anything more generic or clever runs into problems with conflicting instances.
instance QMul Float         Float         Float         where (.*.) = (*)
instance QMul Float         (Qty a)       (Qty a)       where x .*. (Qty y) = Qty (x*y)
instance QMul (Qty a)       Float         (Qty a)       where (Qty x) .*. y = Qty (x*y)
instance QMul MassFraction  Mass          Mass          where (.*.) = qmul
instance QMul Mass          MassFraction  Mass          where (.*.) = qmul
instance QMul MassFraction  MassFraction  MassFraction  where (.*.) = qmul
instance QMul EnergyDensity Mass          Energy        where (.*.) = qmul
instance QMul Mass          EnergyDensity Energy        where (.*.) = qmul
instance QMul MassFraction  EnergyDensity EnergyDensity where (.*.) = qmul
instance QMul EnergyDensity MassFraction  EnergyDensity where (.*.) = qmul

instance QDiv Float         Float         Float         where (./.) = (/)
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
qShow :: NonEmpty (Qty a, String) -> Qty a -> String
qShow units q = T.showSigDigits 3 (q ./. uval) ++ " " ++ uname
  where
    smallest = NE.minimum1 units
    smaller = N.filter ((<= q) . fst) units
    -- Largest unit no larger than q, or the smallest unit if q is smaller than all others:
    largest = NE.maximum1 (smallest :| smaller)
    (uval, uname) = largest

instance T.PrettyShow Mass where
    prettyShow = qShow $ (ug, "µg") :| [(mg, "mg"), (g, "g"), (kg, "kg")]
instance T.PrettyShow Energy where
    prettyShow = qShow $ (kcal, "kcal") :| []
instance T.PrettyShow Time where
    prettyShow = qShow $ (day, "days") :| []
instance T.PrettyShow MassFraction where
    prettyShow x = T.prettyShow (x .* 100 .*. g) ++ " / 100 g"
instance T.PrettyShow EnergyDensity where
    prettyShow x = T.prettyShow (x .* 100 .*. g) ++ " / 100 g"
