module Ibis.Data.Zip (
    unzipAll,
    zipContents,
) where

import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Interpret the input as the contents of a zip archive, and unzip every file inside.
unzipAll :: B.ByteString -> [B.ByteString]
unzipAll = fmap (B.toStrict . Z.fromEntry) . Z.zEntries . Z.toArchive . B.fromStrict

-- | Create a zip archive containing a single file.
zipContents :: FilePath -> String -> B.ByteString
zipContents fileName contents = B.toStrict $ Z.fromArchive archive
  where
    entry = Z.toEntry fileName 0 (BL.pack contents)
    archive = Z.addEntryToArchive entry Z.emptyArchive
