{-# LANGUAGE OverloadedStrings #-}
module Audio.MusicBrainz.Testing.XML (specs) where

import Test.Hspec
import Text.XML.Enumerator.Cursor (Cursor)
import Data.ISO3166_CountryCodes
import Audio.MusicBrainz.Testing.Ext
import Audio.MusicBrainz.Types
import Audio.MusicBrainz.XML

specs ::  IO [IO Spec]
specs = descriptions [parseArtist_full, parseArtist_min]

parseArtist_full :: IO [IO Spec]
parseArtist_full = do actual <- parseArtist . firstArtist =<< readFixture "artist_full.xml"
                      describe 
                        "full Artist profile" 
                        [ it "parses the full artist" $ actual == fullArtist ]

parseArtist_min :: IO [IO Spec]
parseArtist_min = do actual <- parseArtist . firstArtist =<< readFixture "artist_min.xml"
                     describe 
                       "minimal Artist profile" 
                       [ it "parses the minimal artist" $ actual == minimalArtist ]

---- Helpers

firstArtist :: Cursor -> Cursor
firstArtist root = head $ root !<//.> ["artist-list", "artist"]

---- Data Fixtures

fullArtist :: Artist
fullArtist = Artist { artistId             = "aaaaaa-aaaa-aaaa-aaaa-aaaaaaa",
                      artistName           = "Dr. Funke's 100% Natural Good-Time Family-Band Solution",
                      artistType           = "group",
                      artistSortName       = Just "Dr. Funke's 100% Natural Good-Time Family-Band Solution",
                      artistLifeSpan       = (Just pd1, Just pd2),
                      artistAliases        = ["The Funke Bunch", "The Teamocil Band"],
                      artistGender         = Just "Male",
                      artistCountry        = Just US,
                      artistDisambiguation = Just "Good-Time Family-Band Solution",
                      --TODO: fill these in in the fixture
                      artistRecordings     = [fullRecording],
                      artistReleases       = [],
                      artistLabels         = [],
                      artistWorks          = [],
                      artistRelationLists  = [],
                      artistRating         = Just Rating { ratingVotes = 23, ratingScore = 4.3},
                      artistUserRating     = Nothing,
                      artistTags           = [Tag "prescription folk", Tag "family band", UserTag "arrested development bands", UserTag "broken up"] }
  where
        pd1 = PartialDate 1993 (Just 5) (Just 25)
        pd2 = PartialDate 1997 Nothing Nothing

minimalArtist :: Artist
minimalArtist = Artist { artistId             = "aaaaaa-aaaa-aaaa-aaaa-aaaaaaa",
                         artistName           = "Dr. Funke's 100% Natural Good-Time Family-Band Solution",
                         artistType           = "group",
                         artistSortName       = Nothing,
                         artistLifeSpan       = (Nothing, Nothing),
                         artistAliases        = [],
                         artistGender         = Nothing,
                         artistCountry        = Nothing,
                         artistDisambiguation = Nothing,
                         --TODO: fill these in in the fixture
                         artistRecordings     = [],
                         artistReleases       = [],
                         artistLabels         = [],
                         artistWorks          = [],
                         artistRelationLists  = [],
                         artistRating         = Nothing,
                         artistUserRating     = Nothing,
                         artistTags           = [] }

fullRecording :: Recording
fullRecording = Recording { recordingId             = "bbbbbb-bbbb-bbbb-bbbb-bbbbbbb",
                            recordingTitle          = "Timosil",
                            --TODO
                            recordingLength         = Nothing,
                            recordingDisambiguation = Nothing,
                            recordingArtistCredit   = [],
                            recordingReleases       = [],
                            recordingIdentifiers    = [],
                            recordingRelationLists  = [],
                            recordingTags           = [],
                            recordingRating         = Nothing,
                            recordingUserRating     = Nothing }
