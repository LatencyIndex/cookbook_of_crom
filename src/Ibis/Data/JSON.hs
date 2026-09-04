module Ibis.Data.JSON (
    trimArray,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- | Drop everything outside the outer-most [],
-- so that only a JSON array remains as the top-most value.
trimArray :: B.ByteString -> B.ByteString
trimArray = C.dropWhileEnd (/= ']') . C.dropWhile (/= '[')
