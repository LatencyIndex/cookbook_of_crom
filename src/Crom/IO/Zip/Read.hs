module Crom.IO.Zip.Read (
    unzipAll,
) where

import Codec.Archive.Zip qualified as Z
import Data.ByteString qualified as B

-- | Interpret the input as the contents of a zip archive, and unzip every file inside.
unzipAll :: B.ByteString -> [B.ByteString]
unzipAll = fmap (B.toStrict . Z.fromEntry) . Z.zEntries . Z.toArchive . B.fromStrict
