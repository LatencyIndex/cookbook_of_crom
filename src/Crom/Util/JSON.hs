module Crom.Util.JSON (trimArray) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- Drop everything outside the outer-most []
trimArray :: B.ByteString -> B.ByteString
trimArray = C.dropWhileEnd (/= ']') . C.dropWhile (/= '[')
