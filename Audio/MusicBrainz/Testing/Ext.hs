{-# LANGUAGE FlexibleInstances #-}
module Audio.MusicBrainz.Testing.Ext (readFixture) where

import Test.Hspec.Core
import Text.XML.Enumerator.Resolved (readFile_)
import Text.XML.Enumerator.Parse (decodeEntities)
import Text.XML.Enumerator.Cursor (fromDocument, Cursor)

readFixture :: FilePath -> IO Cursor
readFixture p = fromDocument `fmap` readFile_ path decodeEntities
  where path = "Audio/MusicBrainz/Testing/fixtures/" ++ p

instance SpecVerifier (IO Bool) where
  it description example = do
    example' <- example
    r <- safely (if example' then Success else Fail "")
    return (description, r)
