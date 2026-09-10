-- | Module for compiling the food nutrient database, and serializing it.
module Ibis.Scribe (
    scribeDatabase,
) where

-- | Create a nutrient database from the given food sources.
scribeDatabase :: [FilePath] -> FilePath -> IO ()
scribeDatabase srcs dst = undefined -- TODO
