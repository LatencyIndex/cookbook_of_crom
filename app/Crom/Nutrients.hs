module Crom.Nutrients where

-- Nutrients need database with recommended daily dose, etc.
-- TODO: put omega3 fatty acids somewhere.
data Nutrient
    -- Macronutrients
    = Carbohydrate
    | Fat
    | Fibre
    | Protein -- Undefined whether complete or not.
    | CompleteProtein
    -- Minerals/elements
    | Element_B
    | Element_C
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
    -- Protein amino acids
    | Amino_Histidine
    | Amino_Isoleucine
    | Amino_Leucine
    | Amino_Lysine
    | Amino_MethionineCystine
    | Amino_PheylalanineTyrosine
    | Amino_Threonine
    | Amino_Tryptophan
    | Amino_Valine
    -- Vitamins
    | Vitamin_A
    | Vitamin_B12
    | Vitamin_C
    | Vitamin_D
    | Vitamin_E
    | Vitamin_K
    -- Misc
    | Collagen
    | Amino_Glycine
    | Amino_Proline
    | Qenzyme_Q10
    | Creatine
    deriving (Show, Eq, Ord)
