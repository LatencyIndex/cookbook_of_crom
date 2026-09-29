module Crom.Nutrients where

-- Nutrients need database with recommended daily dose, etc.
data Nutrient
    -- Macronutrients
    = Carbohydrates -- Including sugars
    | Sugars
    | Fat
    | Fiber
    | Protein -- Both complete and incomplete
    | CompleteProtein
    -- Minerals/elements (ignoring carbon)
    | Element_B
    | Element_Ca
    | Element_Co
    | Element_Cu
    | Element_F
    | Element_Fe
    | Element_I
    | Element_K
    | Element_Mg
    | Element_Mn
    | Element_Mo
    | Element_Na
    | Element_P
    | Element_S
    | Element_Se
    | Element_Zn
    -- Essential amino acids for protein
    | Amino_Cysteine
    | Amino_Histidine
    | Amino_Isoleucine
    | Amino_Leucine
    | Amino_Lysine
    | Amino_Methionine
    | Amino_Phenylalanine
    | Amino_Threonine
    | Amino_Tryptophan
    | Amino_Tyrosine
    | Amino_Valine
    -- Omega fatty acids
    | Omega3
    | Omega6
    | Omega7
    | Omega9
    -- Vitamins
    | Vitamin_A
    | Vitamin_B1
    | Vitamin_B2
    | Vitamin_B3
    | Vitamin_B5
    | Vitamin_B6
    | Vitamin_B7
    | Vitamin_B9
    | Vitamin_B12
    | Vitamin_C
    | Vitamin_D
    | Vitamin_E
    | Vitamin_K
    -- Misc
    | Cholesterol
    | Choline
    | Coenzyme_Q10
    | Collagen
    | Creatine
    deriving (Show, Read, Eq, Ord)
