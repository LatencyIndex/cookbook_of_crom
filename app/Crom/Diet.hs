module Crom.Diet where

import qualified Data.Map as M
import qualified Data.Text as T
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

data BiologicalRole
    = Essential
    | Beneficial
    | Uncertain
    | Unnecessary

data RecommendedDailyIntake = RDI {
    role :: BiologicalRole,
    comment :: T.Text,
    min :: Maybe U.Mass,
    mid :: Maybe U.Mass,
    max :: Maybe U.Mass
}

-- For healthy adult male
recommendedDailyIntake :: U.Mass -> M.Map N.Nutrient RecommendedDailyIntake
recommendedDailyIntake bodymass =
    let kgBm = U.to_kg bodymass :: Float
        -- Convenience functions
        just_g :: Float -> Maybe U.Mass
        just_g = Just . U.as_g
        just_mg :: Float -> Maybe U.Mass
        just_mg = Just . U.as_mg
        just_ug :: Float -> Maybe U.Mass
        just_ug = Just . U.as_ug
    in M.fromList [
    -- -- Macronutrients
    -- (N.Carbohydrate),
    -- (N.Fat),
    -- According to EFSA: https://en.wikipedia.org/wiki/Dietary_fiber#Dietary_recommendations
    (N.Fibre, RDI Beneficial ""
        Nothing (just_g 25) Nothing),
    -- (N.Protein),
    -- Even 1.5g protein/kg bodymass is near-optimal: https://mennohenselmans.com/the-myth-of-1glb-optimal-protein-intake-for-bodybuilders/
    (N.CompleteProtein, RDI Beneficial "near-optimal even at lower limit"
        (just_g (1.5 * kgBm)) (just_g (1.8 * kgBm)) Nothing),
    -- -- Minerals/elements
    -- https://en.wikipedia.org/wiki/Boron#Biological_role
    (N.Element_B, RDI Uncertain "Lower limit is very uncertain."
        Nothing Nothing (just_mg 20)),
    -- Not sure what to do about carbon lol
    -- (N.Element_C),
    -- https://en.wikipedia.org/wiki/Calcium#Biological_and_pathological_role
    (N.Element_Ca, RDI Essential ""
        Nothing (just_g 1) Nothing),
    -- https://en.wikipedia.org/wiki/Cobalt#Biological_role
    (N.Element_Co, RDI Unnecessary "Co requirements fulfilled by vitamin B12"
        Nothing Nothing Nothing),
    -- https://en.wikipedia.org/wiki/Copper#Dietary_recommendations
    (N.Element_Cu, RDI Essential ""
        Nothing (just_mg 1.6) (just_mg 5)),
    -- https://en.wikipedia.org/wiki/Fluorine#Biological_role
    (N.Element_F, RDI Unnecessary "present in toothpaste, only needed for tooth enamel"
        Nothing Nothing Nothing),
    -- https://en.wikipedia.org/wiki/Iron#Nutrition
    (N.Element_Fe, RDI Essential ""
        Nothing (just_mg 8) (just_mg 45)),
    -- https://en.wikipedia.org/wiki/Iodine#Dietary_intake
    (N.Element_I, RDI Essential ""
        (just_ug 70) (just_ug 150) (just_ug 1100)),
    -- https://en.wikipedia.org/wiki/Potassium#Nutrition
    (N.Element_K, RDI Essential ""
        Nothing (just_g 3.4) Nothing),
    -- https://en.wikipedia.org/wiki/Magnesium#Nutrition
    (N.Element_Mg, RDI Essential ""
        Nothing (just_mg 420) Nothing),
    -- https://en.wikipedia.org/wiki/Manganese#Nutrition
    (N.Element_Mn, RDI Essential ""
        Nothing (just_mg 3) (just_mg 11)),
    -- https://en.wikipedia.org/wiki/Molybdenum#Dietary_recommendations
    (N.Element_Mo, RDI Essential ""
        Nothing (just_ug 65) (just_ug 600)),
    -- May not apply if you lose a lot of Na through sweat:
    -- https://www.heart.org/en/healthy-living/healthy-eating/eat-smart/sodium/how-much-sodium-should-i-eat-per-day
    (N.Element_Na, RDI Essential "should consume more if you lose a lot of Na through sweat"
        (just_g 0.5) Nothing (just_g 1.5))
    -- https://en.wikipedia.org/wiki/Phosphorus#Nutrition
    (N.Element_P, RDI Essential "if a diet has sufficient protein and calcium, the amount of phosphorus is probably sufficient"
        Nothing (just_mg 550) (just_mg 4000)),
    -- https://en.wikipedia.org/wiki/Sulfur#Biological_role
    (N.Element_S, RDI Essential "if a diet has sufficient protein, the amount of sulfur is probably sufficient"
        Nothing Nothing Nothing),
    -- https://en.wikipedia.org/wiki/Selenium#Nutritional_sources_of_selenium
    (N.Element_Se, RDI Essential ""
        Nothing (just_ug 55) (just_ug 400)),
    -- https://en.wikipedia.org/wiki/Zinc#Nutrition
    (N.Element_Zn, RDI Essential "US says max is 40mg, EU says 25mg"
        Nothing (just_mg 11) (just_mg 25)),
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
