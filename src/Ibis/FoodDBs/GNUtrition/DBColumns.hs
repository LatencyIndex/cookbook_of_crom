-- | Column names of the GNUtrition 'nutrients' table.
module Ibis.FoodDBs.GNUtrition.DBColumns (
    equivalences,
    nutrients,
    name,
    foodCode,
    calories,
) where

import Crom.Nutrients qualified as N
import Data.Map.Strict qualified as M
import Data.Text qualified as T

name :: T.Text
name = "Main food description"

foodCode :: T.Text
foodCode = "Food code"

calories :: T.Text
calories = "Energy (kcal)"

_miscKeys :: [T.Text]
_miscKeys =
    [ "WWEIA Category number",
      "WWEIA Category description"
    ]

-- | Nutrients as tracked by Crom, and the weighted sum of GNUtrition nutrients it corresponds to.
equivalences :: M.Map N.Nutrient [(Float, T.Text)]
equivalences = M.fromList [
    (N.Carbohydrates,       [(1, "Carbohydrate (g)")]),
    (N.Sugars,              [(1, "Sugars, total\n(g)")]),
    (N.Fat,                 [(1, "Total Fat (g)")]),
    (N.Fiber,               [(1, "Fiber, total dietary (g)")]),
    (N.Protein ,            [(1, "Protein (g)")]),
    (N.CompleteProtein,     []),
    (N.Element_B,           []),
    (N.Element_Ca,          [(1, "Calcium (mg)")]),
    (N.Element_Co,          []),
    (N.Element_Cu,          [(1, "Copper (mg)")]),
    (N.Element_F,           []),
    (N.Element_Fe,          [(1, "Iron\n(mg)")]),
    (N.Element_I,           []),
    (N.Element_K,           [(1, "Potassium (mg)")]),
    (N.Element_Mg,          [(1, "Magnesium (mg)")]),
    (N.Element_Mn,          []),
    (N.Element_Mo,          []),
    (N.Element_Na,          [(1, "Sodium (mg)")]),
    (N.Element_P,           [(1, "Phosphorus (mg)")]),
    (N.Element_S,           []),
    (N.Element_Se,          [(1, "Selenium (mcg)")]),
    (N.Element_Zn,          [(1, "Zinc\n(mg)")]),
    (N.Amino_Cysteine,      []),
    (N.Amino_Histidine,     []),
    (N.Amino_Isoleucine,    []),
    (N.Amino_Leucine,       []),
    (N.Amino_Lysine,        []),
    (N.Amino_Methionine,    []),
    (N.Amino_Phenylalanine, []),
    (N.Amino_Threonine,     []),
    (N.Amino_Tryptophan,    []),
    (N.Amino_Tyrosine,      []),
    (N.Amino_Valine,        []),
    (N.Omega3,              []),
    (N.Omega6,              []),
    (N.Omega7,              []),
    (N.Omega9,              []),
    (N.Vitamin_A,           [(1, "Vitamin A, RAE (mcg_RAE)")]),
    (N.Vitamin_B1,          [(1, "Thiamin (mg)")]),
    (N.Vitamin_B2,          [(1, "Riboflavin (mg)")]),
    (N.Vitamin_B3,          [(1, "Niacin (mg)")]),
    (N.Vitamin_B5,          []),
    (N.Vitamin_B6,          [(1, "Vitamin B-6 (mg)")]),
    (N.Vitamin_B7,          []),
    (N.Vitamin_B9,          [(1, "Folate, DFE (mcg_DFE)")]),
    (N.Vitamin_B12,         [(1, "Vitamin B-12 (mcg)"), (1, "Vitamin B-12, added\n(mcg)")]),
    (N.Vitamin_C,           [(1, "Vitamin C (mg)")]),
    (N.Vitamin_D,           [(1, "Vitamin D (D2 + D3) (mcg)")]),
    (N.Vitamin_E,           [(1, "Vitamin E (alpha-tocopherol) (mg)"), (1, "Vitamin E, added\n(mg)")]),
    (N.Vitamin_K,           [(1, "Vitamin K (phylloquinone) (mcg)")]),
    (N.Cholesterol,         [(1, "Cholesterol (mg)")]),
    (N.Choline,             [(1, "Choline, total (mg)")]),
    (N.Coenzyme_Q10,        []),
    (N.Collagen,            []),
    (N.Creatine,            [])]

-- | Nutrients excluded from comparison between GNUtrition and Crom.
ignored :: [T.Text]
ignored = [
    -- My code doesn't track these as such.
    "Fatty acids, total saturated (g)",
    "Fatty acids, total monounsaturated (g)",
    "Fatty acids, total polyunsaturated (g)",
    -- Too ambiguous to bother with for now.
    "4:0\n(g)",
    "6:0\n(g)",
    "8:0\n(g)",
    "10:0\n(g)",
    "12:0\n(g)",
    "14:0\n(g)",
    "16:0\n(g)",
    "18:0\n(g)",
    "16:1\n(g)",
    "18:1\n(g)",
    "20:1\n(g)",
    "22:1\n(g)",
    "18:2\n(g)",
    "18:3\n(g)",
    "18:4\n(g)",
    "20:4\n(g)",
    "20:5 n-3\n(g)",
    "22:5 n-3\n(g)",
    "22:6 n-3\n(g)",
    -- Presumably included under vitamin A.
    "Retinol (mcg)",
    "Cryptoxanthin, beta (mcg)",
    "Carotene, alpha (mcg)",
    "Carotene, beta (mcg)",
    -- Presumably included under "Folate, DFE" (Dietary Folate Equivalent), a.k.a. vitamin B9.
    "Folic acid (mcg)",
    "Folate, food (mcg)",
    "Folate, total (mcg)",
    -- Helps prevent age-related macular degeneration, but we don't track it yet.
    "Lutein + zeaxanthin (mcg)",
    -- Non-essential
    "Caffeine (mg)",
    "Alcohol (g)",
    "Theobromine (mg)",
    "Lycopene (mcg)",
    -- Don't track water.
    "Water\n(g)"]

-- | All nutrient keys/columns names, used by GNUtrition.
nutrients :: [T.Text]
nutrients = concatMap (fmap snd) (M.elems equivalences) ++ ignored
