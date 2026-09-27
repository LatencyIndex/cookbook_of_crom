{-# LANGUAGE FlexibleContexts #-}

module Ibis.FoodDBs.USDA.Compile.Nutrients (
    JSNutrientMap,
    toJSNutrientMap,
    deriveCalories,
    deriveNutrients,
) where

import Control.Arrow ((>>>))
import qualified Crom.Nutrients as N
import Crom.Units ((*.), (.*.))
import qualified Crom.Units as U
import Data.Function (on, (&))
import qualified Data.List as L
import Data.List.Extra (firstJust)
import qualified Data.Map as M
import Data.Monoid (Sum (..))
import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Ibis.FoodDBs.USDA.Compile.PolyQuantity as P
import qualified Ibis.FoodDBs.USDA.Read as R

-- | Map from nutrient name to quantity, as present in the JSON food database.
-- One nutrient may have multiple quantities listed.
type JSNutrientMap = M.Map T.Text [P.PolyQuantity]

toJSNutrientMap :: [R.RawNutrient] -> JSNutrientMap
toJSNutrientMap =
    fmap (\x -> (x.name, [x.quantity])) -- split into key-value pairs
        >>> M.fromListWith (<>) -- create a Map k [v]
        >>> fmap kcalSort -- sort values by priority
        >>> fmap (fmap P.fromRawQuantity) -- parse quantities
  where
    -- Prioritize kcal, then kJ, then everything else.
    kcalFirst :: T.Text -> Int
    kcalFirst "kcal" = 0
    kcalFirst "kJ" = 1
    kcalFirst _ = 2
    kcalSort :: [R.RawQuantity] -> [R.RawQuantity]
    kcalSort = L.sortOn (kcalFirst . (.unit))

toFirstJust :: (a -> Maybe b) -> M.Map k [a] -> M.Map k b
toFirstJust f = M.mapMaybe (firstJust f)

-- 'max' picks the best RankedSum
-- 'mconcat' sums a list of RSums into a RankedSum
data RankedSum a = RankedSum
    { total :: Int, -- Total number of values that were supposed to form this sum.
      present :: Int, -- Total - missing values.
      value :: a
    }

-- | Singleton for RankedSum.
rankOne :: a -> RankedSum a
rankOne = RankedSum 1 1

-- If there is no data, instead of 0/0, 0/1 is used, to avoid division by zero.
-- This still results in correct ranking, since all RankedSums with 0 present values
-- are equivalent.
rank :: RankedSum a -> Rational
rank x = fromIntegral x.present % fromIntegral (max 1 x.total)

toValid :: RankedSum a -> Maybe a
toValid x
    | x.present > 0 = Just x.value
    | otherwise = Nothing

instance Functor RankedSum where
    fmap f (RankedSum total present value) = RankedSum total present (f value)
instance Eq (RankedSum a) where
    (==) = (==) `on` rank
instance Ord (RankedSum a) where
    compare = compare `on` rank
instance (Semigroup a) => Semigroup (RankedSum a) where
    (RankedSum t0 p0 v0) <> (RankedSum t1 p1 v1) = RankedSum (t0 + t1) (p0 + p1) (v0 <> v1)
instance (Monoid a) => Monoid (RankedSum a) where
    mempty = RankedSum 0 0 mempty

get :: (Monoid a, Ord k) => M.Map k a -> k -> RankedSum a
get m k = maybe mempty rankOne (M.lookup k m)

gets :: (Functor f, Monoid a, Ord k) => M.Map k a -> f k -> f (RankedSum a)
gets m = fmap (get m)

getWeighted :: (Monoid a, Ord k, U.QMul Float a a) => M.Map k a -> (Float, k) -> RankedSum a
getWeighted m (w, k) = get m k & fmap (w .*.)

getsWeighted :: (Functor f, Monoid a, Ord k, U.QMul Float a a) => M.Map k a -> f (Float, k) -> f (RankedSum a)
getsWeighted m = fmap (getWeighted m)

sum' :: (Monoid a) => [RankedSum a] -> RankedSum a
sum' = mconcat

-- | Returns the best RankedSum, or the empty RankedSum if the foldable is empty.
maxRank :: (Foldable f, Monoid a) => f (RankedSum a) -> RankedSum a
maxRank = foldl' max mempty

deriveCalories :: JSNutrientMap -> Maybe U.EnergyDensity
deriveCalories nutrientMap =
    let
        energy :: M.Map T.Text U.EnergyDensity
        energy = toFirstJust P.toEnergyDensity nutrientMap
     in
        (toValid . maxRank . gets energy)
            [ "Energy", -- Presumably chosen as most accurate by the analysts.
              "Energy (Atwater Specific Factors)", -- Takes into account absorption.
              "Energy (Atwater General Factors)" -- Ignores absorption.
            ]

deriveNutrients :: JSNutrientMap -> M.Map N.Nutrient U.MassFraction
deriveNutrients nutrientMap =
    let
        mass :: M.Map T.Text U.MassFraction
        mass = toFirstJust P.toMassFraction nutrientMap
        iu :: M.Map T.Text (Sum Float)
        iu = Sum <$> toFirstJust P.toIU nutrientMap
        -- Convenience function to convert international unit to mass fraction.
        mmulIU :: U.MassFraction -> RankedSum (Sum Float) -> RankedSum U.MassFraction
        mmulIU m = fmap ((m .*.) . getSum)
     in
        [ -- By summation is a newer and more accurate method than by difference.
          (N.Carbohydrates, (maxRank . gets mass) ["Carbohydrate, by summation", "Carbohydrate, by difference"]),
          (N.Sugars, (maxRank . gets mass) ["Sugars, Total", "Total Sugars", "Sugars, total including NLEA"]),
          (N.Fat, (maxRank . gets mass) ["Total fat (NLEA)", "Total lipid (fat)"]),
            ( N.Fiber,
              maxRank
                [ get mass "Fiber, total dietary",
                  get mass "Total dietary fiber (AOAC 2011.25)",
                  (sum' . gets mass) ["Fiber, insoluble", "Fiber, soluble"],
                  (sum' . gets mass) ["High Molecular Weight Dietary Fiber (HMWDF)", "Low Molecular Weight Dietary Fiber (LMWDF)"]
                ]
            ),
          -- Total, not necessarily complete.
          (N.Protein, get mass "Protein"),
          -- Has to be calculated from amino acid makeup.
          -- However, it is not additive, so it is more meaningful to calculate it for an entire
          -- meal/day, than for a single food.
          (N.CompleteProtein, mempty),
          (N.Element_B, get mass "Boron, B"),
          (N.Element_Ca, get mass "Calcium, Ca"),
          (N.Element_Co, get mass "Cobalt, Co"),
          (N.Element_Cu, get mass "Copper, Cu"),
          (N.Element_F, get mass "Fluoride, F"),
          (N.Element_Fe, get mass "Iron, Fe"),
          (N.Element_I, get mass "Iodine, I"),
          (N.Element_K, get mass "Potassium, K"),
          (N.Element_Mg, get mass "Magnesium, Mg"),
          (N.Element_Mn, get mass "Manganese, Mn"),
          (N.Element_Mo, get mass "Molybdenum, Mo"),
          (N.Element_Na, get mass "Sodium, Na"),
          (N.Element_P, get mass "Phosphorus, P"),
          (N.Element_S, get mass "Sulfur, S"),
          (N.Element_Se, get mass "Selenium, Se"),
          (N.Element_Zn, get mass "Zinc, Zn"),
          (N.Amino_Cysteine, get mass "Cysteine"),
          (N.Amino_Histidine, get mass "Histidine"),
          (N.Amino_Isoleucine, get mass "Isoleucine"),
          (N.Amino_Leucine, get mass "Leucine"),
          (N.Amino_Lysine, get mass "Lysine"),
          (N.Amino_Methionine, get mass "Methionine"),
          (N.Amino_Phenylalanine, get mass "Phenylalanine"),
          (N.Amino_Threonine, get mass "Threonine"),
          (N.Amino_Tryptophan, get mass "Tryptophan"),
          (N.Amino_Tyrosine, get mass "Tyrosine"),
          (N.Amino_Valine, get mass "Valine"),
            ( N.Omega3,
              (sum' . gets mass)
                [ "PUFA 18:3 n-3 c,c,c (ALA)", -- Alpha-linolenic acid
                  "PUFA 20:3 n-3",
                  "PUFA 20:5 n-3 (EPA)",
                  "PUFA 22:5 n-3 (DPA)",
                  "PUFA 22:6 n-3 (DHA)",
                  "PUFA 2:5 n-3 (EPA)" -- Probably a typo for 20:5
                ]
            ),
            ( N.Omega6,
              (sum' . gets mass)
                [ "PUFA 18:2 n-6 c,c",
                  "PUFA 18:3 n-6 c,c,c",
                  "PUFA 20:2 n-6 c,c",
                  "PUFA 20:3 n-6",
                  "PUFA 2:4 n-6" -- Probably a typo for 2x:4
                ]
            ),
          (N.Omega7, mempty), -- Not in USDA database.
            ( N.Omega9,
              (sum' . gets mass)
                [ "MUFA 22:1 n-9", -- Erucic acid
                  "PUFA 20:3 n-9" -- Mead acid
                ]
            ),
          -- Dosage equivalents at https://en.wikipedia.org/wiki/Vitamin_A#Units_of_measurement
          -- Dietary recommendations given in RAE (retinol activity equivalent)
            ( N.Vitamin_A,
              -- 3.33 IU of vitamin A is equivalent to 1 microgram of RAE
              (((1 / 3.33) *. U.ug_per_100g) `mmulIU` get iu "Vitamin A, IU")
                `max` get mass "Vitamin A, RAE"
                `max` (sum' . getsWeighted mass) [(1, "Retinol"), (1 / 24, "Cryptoxanthin, beta")]
            ),
          (N.Vitamin_B1, get mass "Thiamin"),
          (N.Vitamin_B2, get mass "Riboflavin"),
          (N.Vitamin_B3, get mass "Niacin"),
          (N.Vitamin_B5, get mass "Pantothenic acid"),
          (N.Vitamin_B6, get mass "Vitamin B-6"),
          (N.Vitamin_B7, get mass "Biotin"),
            ( N.Vitamin_B9,
              get mass "Folate, DFE" -- DFE: Dietary folate equivalent.
                `max` ( (maxRank . gets mass) ["Folate, total", "Folate, food"] -- Folate presumably does not include folic acid.
                            <> getWeighted mass (1.7, "Folic acid") -- 1g of folic acid is equivalent to 1.7g DFE.
                      )
            ),
          (N.Vitamin_B12, (sum' . gets mass) ["Vitamin B-12", "Vitamin B-12, added"]),
          (N.Vitamin_C, get mass "Vitamin C, total ascorbic acid"),
          -- Vitamins D2 and D3 are biologically equivalent, others are non-essential.
            ( N.Vitamin_D,
              get mass "Vitamin D (D2 + D3)"
                -- For vitamin D, 1 microgram = 40 IU
                `max` (((1 / 40) *. U.ug_per_100g) `mmulIU` get iu "Vitamin D (D2 + D3), International Units")
                `max` (sum' . gets mass) ["Vitamin D2 (ergocalciferol)", "Vitamin D3 (cholecalciferol)"]
            ),
            ( N.Vitamin_E,
              (sum' . gets mass)
                [ "Tocopherol, beta",
                  "Tocopherol, delta",
                  "Tocopherol, gamma",
                  "Tocotrienol, alpha",
                  "Tocotrienol, beta",
                  "Tocotrienol, delta",
                  "Tocotrienol, gamma",
                  "Vitamin E (alpha-tocopherol)",
                  -- "added" vitamins are separate from other vitamin analysis, and don't overlap.
                  -- Meaning they should be added to the total.
                  "Vitamin E, added"
                ]
            ),
          -- Dihydrophylloquinone (database entry "Vitamin K (Dihydrophylloquinone)")
          -- is associated with lower bone density (which vitamin K is supposed to help)
          -- and I found no information on how effective it is as a vitamin K,
          -- so as a precaution it is excluded from total vitamin K.
          -- Vitamins K1 (phylloquinone) & K2 (menaquinone) are considered equivalent.
          (N.Vitamin_K, (sum' . gets mass) ["Vitamin K (Menaquinone-4)", "Vitamin K (phylloquinone)"]),
          (N.Cholesterol, get mass "Cholesterol"),
          -- In the USDA database, if any form of choline is given, then the choline total is also given.
          -- So we concern ourselves only with the total, and ignore the others.
          (N.Choline, get mass "Choline, total"),
          (N.Coenzyme_Q10, mempty), -- Not in USDA database.
          (N.Collagen, mempty), -- Not in USDA database.
          (N.Creatine, mempty) -- Not in USDA database.
        ]
            & M.fromList
            & M.mapMaybe toValid -- Discard values that have no data points.
