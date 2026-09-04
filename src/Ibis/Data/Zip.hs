module Ibis.Data.Zip (
    unzipAll,
) where

import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString as B

-- | Interpret the input as the contents of a zip archive, and unzip every file inside.
unzipAll :: B.ByteString -> [B.ByteString]
unzipAll = fmap (B.toStrict . Z.fromEntry) . Z.zEntries . Z.toArchive . B.fromStrict
