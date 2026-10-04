module Crom.IO.Zip (
    unzipAll,
    zipContents,
) where

import Codec.Archive.Zip qualified as Z
import Data.ByteString.Lazy.Char8 qualified as BL

-- | Interpret the input as the contents of a zip archive, and unzip every file inside.
unzipAll :: BL.ByteString -> [BL.ByteString]
unzipAll = fmap Z.fromEntry . Z.zEntries . Z.toArchive

-- | Create a zip archive containing a single file.
zipContents :: FilePath -> BL.ByteString -> BL.ByteString
zipContents fileName contents = Z.fromArchive archive
  where
    entry = Z.toEntry fileName 0 contents
    archive = Z.addEntryToArchive entry Z.emptyArchive
