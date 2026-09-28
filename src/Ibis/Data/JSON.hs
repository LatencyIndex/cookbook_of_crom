module Ibis.Data.JSON (
    trimArray,
) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C

-- | Drop everything outside the outer-most [],
-- so that only a JSON array remains as the top-most value.
trimArray :: B.ByteString -> B.ByteString
trimArray = C.dropWhileEnd (/= ']') . C.dropWhile (/= '[')
