-- | Convert raw food data into useful form.
module Ibis.Compile (
    toFood,
) where

import qualified Crom.Food as F
import qualified Ibis.Read as R

toFood :: R.RawFood -> F.Food
toFood = undefined -- TODO
