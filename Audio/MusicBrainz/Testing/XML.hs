{-# LANGUAGE OverloadedStrings #-}
module Audio.MusicBrainz.Testing.XML (specs) where

import Test.Hspec
import Text.XML.Enumerator.Cursor (Cursor)
import Data.ISO3166_CountryCodes
import Audio.MusicBrainz.Testing.Ext
import Audio.MusicBrainz.Types
import Audio.MusicBrainz.XML

--DEBUG
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Text.XML.Enumerator.Cursor
import Data.Maybe

specs ::  IO [IO Spec]
--specs = descriptions [parseArtist_full, parseArtist_min]
specs = descriptions [parseArtist_min]

parseArtist_full :: IO [IO Spec]
parseArtist_full = do fullArtist <- parseArtist . firstArtist =<< readFixture "artist_full.xml"
                      putStrLn . show $ fullArtist
                      describe 
                        "full Artist profile" 
                        [ it "parses the full artist" $ fullArtist == expectedArtist ]
  where expectedArtist = Artist { artistId             = "aaaaaa-aaaa-aaaa-aaaa-aaaaaaa",
                                  artistName           = "Dr. Funke's 100% Natural Good-Time Family-Band Solution",
                                  artistType           = "group",
                                  artistSortName       = Just "Dr. Funke's 100% Natural Good-Time Family-Band Solution",
                                  artistLifeSpan       = (Just pd1, Just pd2),
                                  artistAliases        = ["The Funke Bunch", "The Teamocil Band"],
                                  artistGender         = Just "Male",
                                  artistCountry        = Just US,
                                  artistDisambiguation = Just "Good-Time Family-Band Solution",
                                  --TODO: fill these in in the fixture
                                  artistRecordings   = [],
                                  artistReleases     = [],
                                  artistLabels       = [],
                                  artistWorks        = [],
                                  artistRelationLists= [],
                                  artistUserTags     = [],
                                  artistRating       = Just Rating { ratingVotes = 23, ratingScore = 4.3},
                                  artistUserRating   = Nothing,
                                  artistTags           = [Tag "prescription folk", Tag "family band"] }
        pd1 = PartialDate 1993 (Just 5) (Just 25)
        pd2 = PartialDate 1997 Nothing Nothing

parseArtist_min :: IO [IO Spec]
parseArtist_min = do fullArtist <- parseArtist . firstArtist =<< readFixture "artist_min.xml"
                     putStrLn . show $ fullArtist
                     describe 
                       "minimal Artist profile" 
                       [ it "parses the minimal artist" $ fullArtist == expectedArtist ]
  where expectedArtist = Artist { artistId             = "aaaaaa-aaaa-aaaa-aaaa-aaaaaaa",
                                  artistName           = "Dr. Funke's 100% Natural Good-Time Family-Band Solution",
                                  artistType           = "group",
                                  artistSortName       = Nothing,
                                  artistLifeSpan       = (Nothing, Nothing),
                                  artistAliases        = [],
                                  artistGender         = Nothing,
                                  artistCountry        = Nothing,
                                  artistDisambiguation = Nothing,
                                  --TODO: fill these in in the fixture
                                  artistRecordings   = [],
                                  artistReleases     = [],
                                  artistLabels       = [],
                                  artistWorks        = [],
                                  artistRelationLists= [],
                                  artistUserTags     = [],
                                  artistRating       = Nothing,
                                  artistUserRating   = Nothing,
                                  artistTags           = [] }

firstArtist :: Cursor -> Cursor
firstArtist root = head $ deepPath root ["artist-list", "artist"]
