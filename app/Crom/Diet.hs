module Crom.Diet where

import qualified Data.Map as M
import qualified Crom.Nutrients as N
import qualified Crom.Units as U

data Target
    = Cut -- Lose weight.
    | Maintain
    | Bulk -- Build muscle.

dailyKcal :: Target -> U.Mass -> U.Energy
dailyKcal Maintain bodymass = U.asKcal (35 * U.to_kg bodymass)
-- Stronglifts says 10-15% more than TDEE: https://stronglifts.com/gain-weight/#Can_I_gain_weight_without_gaining_fat
-- Start with 10%, then increase as needed, to minimize fat gain.
dailyKcal Bulk bodymass = 1.10 `U.qMul` dailyKcal Maintain bodymass
-- Gaining muscle while losing weight is possible according to https://mennohenselmans.com/gain-muscle-and-lose-fat-at-the-same-time/
-- 500-1000 calorie deficit recommended for weight loss, but 500 is plenty, especially when also training.
dailyKcal Cut bodymass = dailyKcal Maintain bodymass `U.qSub` U.asKcal 500

-- Recommended daily intake
data RDI = RDI {
    min :: Maybe U.Mass,
    mid :: Maybe U.Mass,
    max :: Maybe U.Mass
}

recommendedDailyIntake :: U.Mass -> M.Map N.Nutrient RDI
recommendedDailyIntake bodymass =
    let kgBm = U.to_kg bodymass :: Float
    in M.fromList [
    -- -- Macronutrients
    -- (N.Carbohydrate),
    -- (N.Fat),
    (N.Fibre, RDI Nothing (Just $ U.as_g 25) Nothing), -- According to EFSA: https://en.wikipedia.org/wiki/Dietary_fiber#Dietary_recommendations
    -- (N.Protein),
    -- Even 1.5g protein/kg bodymass is near-optimal: https://mennohenselmans.com/the-myth-of-1glb-optimal-protein-intake-for-bodybuilders/
    (N.CompleteProtein, RDI Nothing (Just $ U.as_g (1.8 * kgBm)) Nothing),
    -- -- Minerals/elements
    -- (N.Element_B),
    -- (N.Element_C),
    -- (N.Element_Ca),
    -- (N.Element_Co),
    -- (N.Element_Cu),
    -- (N.Element_F),
    -- (N.Element_Fe),
    -- (N.Element_I),
    -- (N.Element_K),
    -- (N.Element_Mg),
    -- (N.Element_Mn),
    -- (N.Element_Mo),
    -- May not apply if you lose a lot of Na through sweat:
    -- https://www.heart.org/en/healthy-living/healthy-eating/eat-smart/sodium/how-much-sodium-should-i-eat-per-day
    (N.Element_Na, RDI (Just $ U.as_g 0.5) Nothing (Just $ U.as_g 1.5))
    -- (N.Element_P),
    -- (N.Element_S),
    -- (N.Element_Se),
    -- (N.Element_Zn),
    -- -- Protein amino acids
    -- (N.Amino_Histidine),
    -- (N.Amino_Isoleucine),
    -- (N.Amino_Leucine),
    -- (N.Amino_Lysine),
    -- (N.Amino_MethionineCystine),
    -- (N.Amino_PheylalanineTyrosine),
    -- (N.Amino_Threonine),
    -- (N.Amino_Tryptophan),
    -- (N.Amino_Valine),
    -- -- Vitamins
    -- (N.Vitamin_A),
    -- (N.Vitamin_B12),
    -- (N.Vitamin_C),
    -- (N.Vitamin_D),
    -- (N.Vitamin_E),
    -- (N.Vitamin_K),
    -- -- Misc
    -- (N.Collagen),
    -- (N.Amino_Glycine),
    -- (N.Amino_Proline),
    -- (N.Qenzyme_Q10),
    -- (N.Creatine)
    ]
