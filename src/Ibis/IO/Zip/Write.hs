module Ibis.IO.Zip.Write (
    zipContents,
) where

import Codec.Archive.Zip qualified as Z
import Data.ByteString qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL

-- | Create a zip archive containing a single file.
zipContents :: FilePath -> String -> B.ByteString
zipContents fileName contents = B.toStrict $ Z.fromArchive archive
  where
    entry = Z.toEntry fileName 0 (BL.pack contents)
    archive = Z.addEntryToArchive entry Z.emptyArchive
