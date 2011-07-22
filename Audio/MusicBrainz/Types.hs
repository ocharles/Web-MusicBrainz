--------------------------------------------------------------------
-- |
-- Module      : Audio.MusicBrainz.Types
-- Description : Types returned by the Campfire API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Audio.MusicBrainz.Types (Artist(..),
                                PartialDate(..),
                                Label(..),
                                Rating(..),
                                UserRating(..),
                                Recording(..),
                                Release(..),
                                ReleaseGroup(..),
                                Work(..),
                                Quality(..),
                                Tag(..),
                                LifeSpan(..),
                                NameCredit(..),
                                Asin(..),
                                TextRepresentation(..),
                                Identifier(..)
                                ) where

import Data.Text (Text)
import Data.ISO3166_CountryCodes

type LifeSpan = (Maybe PartialDate, Maybe PartialDate)
data PartialDate = PartialDate Int (Maybe Int) (Maybe Int) deriving (Show, Eq)

---- Core Resources
-- the rng/talking to the devs revealed that artists, recordings, releases, release-groups, works can only be guaranteed to have a name/title and id, everything else is optional and cannot be relied upon
--TODO: most of these should be mabe
data Artist = Artist { artistId             :: Text,
                       artistType           :: Text,
                       artistName           :: Text,
                       artistAliases        :: [Text],
                       artistSortName       :: Maybe Text,
                       artistLifeSpan       :: LifeSpan,
                       artistCountry        :: Maybe CountryCode,
                       artistGender         :: Maybe Text,
                       artistDisambiguation :: Maybe Text,
                       artistRecordings     :: [Recording],
                       artistReleases       :: [Release],
                       artistLabels         :: [Label],
                       artistWorks          :: [Work],
                       artistRelationLists  :: [[Relation]], -- Iffy
                       artistRating         :: Maybe Rating,
                       artistUserRating     :: Maybe UserRating,
                       artistTags           :: [Tag] } deriving (Show, Eq)

data Label = Label deriving (Show, Eq)

data Recording = Recording { recordingId             :: Text,
                             recordingTitle          :: Text,
                             recordingLength         :: Maybe Int,
                             recordingDisambiguation :: Maybe Text,
                             recordingArtistCredit   :: [NameCredit],
                             recordingReleases       :: [Release],
                             recordingIdentifiers    :: [Identifier],
                             recordingRelationLists  :: [[Relation]], -- Iffy
                             recordingTags           :: [Tag],
                             recordingRating         :: Maybe Rating,
                             recordingUserRating     :: Maybe UserRating } deriving (Show, Eq)

data Release = Release { releaseId :: Text,
                         releaseTitle :: Text,
                         releaseStatus :: Maybe Text,
                         releaseQuality :: Maybe Quality,
                         releaseDisambiguation :: Maybe Text,
                         releasePackaging :: Maybe Text,
                         releaseTextRepresentation :: Maybe TextRepresentation, -- language, script
                         releaseArtistCredit :: [NameCredit],
                         --TODO: releaseGroup
                         releaseDate :: Maybe PartialDate,
                         releaseCountry :: Maybe CountryCode,
                         releaseBarcode :: Maybe Text,
                         releaseAsin :: Maybe Asin,
                         --TODO: label info
                         --TODO: medium info
                         relationLists :: [[Relation]] } deriving (Show, Eq)

data ReleaseGroup = ReleaseGroup deriving (Show, Eq)

data Work = Work deriving (Show, Eq)

---- Non-Core Resources
data NameCredit = NameCredit { nameCreditJoinPhrase :: Maybe Text,
                               nameCreditName       :: Text } deriving (Show, Eq)

-- Ratings are 1-5

--TODO: ISO 639 and 15294, respectively
data TextRepresentation = TextRepresentation { textRepresentationLanguage :: Maybe Text,
                                               textRepresentationScript :: Maybe Text } deriving (Show, Eq)
data Rating = Rating { ratingVotes :: Int, 
                       ratingScore :: Float} deriving (Show, Eq)

data UserRating = UserRating { userRatingVotes :: Int, 
                               userRatingScore :: Float} deriving (Show, Eq)

data Quality = Low | Normal | High deriving (Show, Eq)

data Relation = Relation deriving (Show, Eq)

data Tag = Tag { tagName :: Text } | 
           UserTag { tagName :: Text } deriving (Show, Eq)

data Collection = Collection

data Identifier = DiscID Text | 
                  PUID Text |
                  ISRC Text |
                  ISWC Text deriving (Show, Eq)

data Asin = Asin Text deriving (Show, Eq)
