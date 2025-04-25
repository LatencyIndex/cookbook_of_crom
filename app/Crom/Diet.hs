module Crom.Diet where

import qualified Data.Map as M
import qualified Data.Text as T
import Crom.Nutrients (Nutrient(..))
import Crom.Units

data Target
    = Cut -- Lose weight.
    | Maintain
    | Bulk -- Build muscle.

daily_kcal :: Target -> Mass -> Energy
daily_kcal Maintain bodymass = 35 *. kcal_per_kg .*. bodymass
-- Stronglifts says 10-15% more than TDEE: https://stronglifts.com/gain-weight/#Can_I_gain_weight_without_gaining_fat
-- Start with 10%, then increase as needed, to minimize fat gain.
daily_kcal Bulk bodymass = 1.10 *. daily_kcal Maintain bodymass
-- Gaining muscle while losing weight is possible according to https://mennohenselmans.com/gain-muscle-and-lose-fat-at-the-same-time/
-- 500-1000 calorie deficit recommended for weight loss, but 500 is plenty, especially when also training.
daily_kcal Cut bodymass = daily_kcal Maintain bodymass .-. (500 *. kcal)

data BiologicalRole
    = Essential
    | Beneficial
    | Uncertain
    | Unnecessary

data RecommendedDailyIntake = RDI {
    role :: BiologicalRole,
    comment :: T.Text,
    min :: Maybe Mass,
    mid :: Maybe Mass,
    max :: Maybe Mass
}

-- Mass fractions of amino acids required for one mass unit of complete protein.
-- I.e. Protein should contain at least 55mg/g of Leucine (among others) to be 'complete'.
-- Source: https://en.wikipedia.org/wiki/Complete_protein#Amino_acid_profile
-- There is confusion regarding cysteine and cystine:
-- WHO/FAO/UNU Expert Consultation Protein and amino acid requirements in human nutrition (https://pubmed.ncbi.nlm.nih.gov/18330140)
    -- lists cysteine as essential (though it can be replaced by methionine)
-- https://en.wikipedia.org/wiki/Complete_protein
    -- has two tables, uses cysteine in one, and cystine in the other
-- https://vegetarianism.stackexchange.com/questions/1402/what-calculation-provided-the-amino-acid-profile-of-a-complete-protein
    -- uses cysteine
-- Tryptophan                 7 *. mg_per_g
-- Threonine                 27 *. mg_per_g
-- Isoleucine                25 *. mg_per_g
-- Leucine                   55 *. mg_per_g
-- Lysine                    51 *. mg_per_g
-- Methionine + Cysteine     25 *. mg_per_g
-- Phenylalanine + Tyrosine  47 *. mg_per_g
-- Valine                    32 *. mg_per_g
-- Histidine                 18 *. mg_per_g

-- For healthy adult male
-- TODO: omega fatty acids
recommended_daily_intake :: Mass -> M.Map Nutrient RecommendedDailyIntake
recommended_daily_intake bodymass =
    -- Convenience definitions
    let just_g, just_mg, just_ug :: Float -> Maybe Mass
        just_g  = Just . (g .*)
        just_mg = Just . (mg .*)
        just_ug = Just . (ug .*)
    in M.fromList [
    -- Macronutrients
    -- (Carbohydrates),
    -- (Sugars),
    -- (Fat),
    -- According to EFSA: https://en.wikipedia.org/wiki/Dietary_fiber#Dietary_recommendations
    (Fiber, RDI Beneficial ""
        Nothing (just_g 25) Nothing),
    -- (Protein),
    -- Even 1.5g protein/kg bodymass is near-optimal: https://mennohenselmans.com/the-myth-of-1glb-optimal-protein-intake-for-bodybuilders/
    (CompleteProtein, RDI Beneficial "Near-optimal even at lower limit."
        (Just (1.5 *. g_per_kg .*. bodymass)) (Just (1.8 *. g_per_kg .*. bodymass)) Nothing),
    -- Minerals/elements
    -- https://en.wikipedia.org/wiki/Boron#Biological_role
    (Element_B, RDI Uncertain "Lower limit is very uncertain." Nothing Nothing (just_mg 20)),
    -- Not sure what to do about carbon lol
    -- (Element_C),
    -- https://en.wikipedia.org/wiki/Calcium#Biological_and_pathological_role
    (Element_Ca, RDI Essential "" Nothing (just_g 1) Nothing),
    -- https://en.wikipedia.org/wiki/Cobalt#Biological_role
    (Element_Co, RDI Unnecessary "Co requirements fulfilled by vitamin B12" Nothing Nothing Nothing),
    -- https://en.wikipedia.org/wiki/Copper#Dietary_recommendations
    (Element_Cu, RDI Essential "" Nothing (just_mg 1.6) (just_mg 5)),
    -- https://en.wikipedia.org/wiki/Fluorine#Biological_role
    (Element_F, RDI Unnecessary "Present in toothpaste, only needed for tooth enamel."
        Nothing Nothing Nothing),
    -- https://en.wikipedia.org/wiki/Iron#Nutrition
    (Element_Fe, RDI Essential "" Nothing (just_mg 8) (just_mg 45)),
    -- https://en.wikipedia.org/wiki/Iodine#Dietary_intake
    (Element_I, RDI Essential "" (just_ug 70) (just_ug 150) (just_ug 1100)),
    -- https://en.wikipedia.org/wiki/Potassium#Nutrition
    (Element_K, RDI Essential "" Nothing (just_g 3.4) Nothing),
    -- https://en.wikipedia.org/wiki/Magnesium#Nutrition
    (Element_Mg, RDI Essential "" Nothing (just_mg 420) Nothing),
    -- https://en.wikipedia.org/wiki/Manganese#Nutrition
    (Element_Mn, RDI Essential "" Nothing (just_mg 3) (just_mg 11)),
    -- https://en.wikipedia.org/wiki/Molybdenum#Dietary_recommendations
    (Element_Mo, RDI Essential "" Nothing (just_ug 65) (just_ug 600)),
    -- May not apply if you lose a lot of Na through sweat:
    -- https://www.heart.org/en/healthy-living/healthy-eating/eat-smart/sodium/how-much-sodium-should-i-eat-per-day
    (Element_Na, RDI Essential "Should consume more if losing a lot of Na through sweat."
        (just_g 0.5) Nothing (just_g 1.5)),
    -- https://en.wikipedia.org/wiki/Phosphorus#Nutrition
    (Element_P, RDI Essential "If a diet has sufficient protein and calcium, the amount of phosphorus is probably sufficient."
        Nothing (just_mg 550) (just_mg 4000)),
    -- https://en.wikipedia.org/wiki/Sulfur#Biological_role
    (Element_S, RDI Essential "If a diet has sufficient protein, the amount of sulfur is probably sufficient."
        Nothing Nothing Nothing),
    -- https://en.wikipedia.org/wiki/Selenium#Nutritional_sources_of_selenium
    (Element_Se, RDI Essential "" Nothing (just_ug 55) (just_ug 400)),
    -- https://en.wikipedia.org/wiki/Zinc#Nutrition
    (Element_Zn, RDI Essential "US max is 40mg, EU is 25mg." Nothing (just_mg 11) (just_mg 25)),
    -- Protein amino acids
    -- (Amino_Cysteine),
    -- (Amino_Histidine),
    -- (Amino_Isoleucine),
    -- (Amino_Leucine),
    -- (Amino_Lysine),
    -- (Amino_Methionine),
    -- (Amino_Phenylalanine),
    -- (Amino_Threonine),
    -- (Amino_Tryptophan),
    -- (Amino_Tyrosine),
    -- (Amino_Valine),
    -- Vitamins: https://en.wikipedia.org/wiki/Vitamin#List_of_vitamins
    (Vitamin_A,   RDI Essential "" Nothing (just_ug 900) (just_ug 3000)),
    (Vitamin_B1,  RDI Essential "" Nothing (just_mg 1.2) Nothing),
    (Vitamin_B2,  RDI Essential "" Nothing (just_mg 1.3) Nothing),
    (Vitamin_B3,  RDI Essential "" Nothing (just_mg 16)  Nothing),
    (Vitamin_B5,  RDI Essential "" Nothing (just_mg 5)   Nothing),
    (Vitamin_B6,  RDI Essential "" Nothing (just_mg 1.3) Nothing),
    (Vitamin_B7,  RDI Essential "" Nothing (just_ug 30)  Nothing),
    (Vitamin_B9,  RDI Essential "" Nothing (just_ug 400) Nothing),
    (Vitamin_B12, RDI Essential "" Nothing (just_ug 2.4) Nothing),
    (Vitamin_C,   RDI Essential "" Nothing (just_mg 90)  Nothing),
    -- Vitamins D2 & D3 are biologically equivalent as far as we know, and others are non-essential,
    -- so only a single value is used for vitamin D content, which represents D2 + D3.
    -- There was a statistical error in the article upon which most vitamin D intake recommendations are based.
    -- The intake should be much higher, ~4000-8000 IU/day:
    -- https://vitamindforall.org/letter.html
    -- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5541280/
    -- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4210929/
    -- https://www.ncbi.nlm.nih.gov/m/pubmed/28768407/
    -- But also this level is allegedly too much according to an anecdotal story:
    --      https://www.devaboone.com/post/vitamin-d-part-2-shannon-s-story
    --      https://news.ycombinator.com/item?id=39744878
    (Vitamin_D,  RDI Essential "" Nothing (just_ug 15)  (just_ug 100)),
    (Vitamin_E,   RDI Essential "" Nothing (just_mg 15)  Nothing),
    (Vitamin_K1,  RDI Essential "" Nothing (just_ug 110) Nothing),
    (Vitamin_K2,  RDI Essential "" Nothing (just_ug 110) Nothing),
    -- Misc
    -- (Cholesterol)
    -- https://en.wikipedia.org/wiki/Choline#Dietary_recommendations
    (Choline, RDI Essential "" Nothing (just_mg 550) Nothing),
    -- https://en.wikipedia.org/wiki/Coenzyme_Q10
    (Coenzyme_Q10, RDI Beneficial "Based on weak evidence. Obtained mostly as supplement."
        Nothing (just_mg 100) (just_mg 1200)),
    -- https://www.webmd.com/diet/collagen-health-benefits
    (Collagen, RDI Beneficial "" Nothing (just_g 2.5) (just_g 15)),
    -- https://examine.com/supplements/Creatine/
    (Creatine, RDI Beneficial "Ideally in monohydrate form. 25g/day during 1st week."
        Nothing (Just (0.06 *. g_per_kg .*. bodymass)) Nothing)
    ]

-- Misc. diet notes:
    -- Avoid soybean oil: https://www.universityofcalifornia.edu/news/americas-most-widely-consumed-cooking-oil-causes-genetic-changes-brain
