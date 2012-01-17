--------------------------------------------------------------------
-- |
-- Module      : Web.MusicBrainz.Types
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
module Web.MusicBrainz.Types (Artist(..),
                                PartialDate(..),
                                Label(..),
                                Direction(..),
                                Rating(..),
                                UserRating(..),
                                Recording(..),
                                Release(..),
                                Relation(..),
                                RelationList(..),
                                ReleaseGroup(..),
                                Work(..),
                                Quality(..),
                                Tag(..),
                                LifeSpan(..),
                                NameCredit(..),
                                ASIN(..),
                                MBID(..),
                                TextRepresentation(..),
                                Identifier(..)
                                ) where

import Data.Text (Text)
import Data.ISO3166_CountryCodes

type LifeSpan = (Maybe PartialDate, Maybe PartialDate)
data PartialDate = PartialDate Int (Maybe Int) (Maybe Int) deriving (Show, Eq)

---- Core Resources
-- the rng/talking to the devs revealed that artists, recordings, releases, release-groups, works can only be guaranteed to have a name/title and id, everything else is optional and cannot be relied upon
--TODO: i think IDs are MBIDs. may have to switch that over

data Artist = Artist { artistId             :: Text,
                       artistType           :: Maybe Text,
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
                       artistRelationLists  :: [RelationList],
                       artistRating         :: Maybe Rating,
                       artistUserRating     :: Maybe UserRating,
                       artistTags           :: [Tag] } deriving (Show, Eq)

data Label = Label { labelId             :: Text,
                     labelType           :: Maybe Text,
                     labelName           :: Text,
                     labelSortName       :: Maybe Text,
                     labelCode           :: Maybe Int,
                     labelDisambiguation :: Maybe Text,
                     labelCountry        :: Maybe CountryCode,
                     labelLifeSpan       :: Maybe LifeSpan,
                     labelAliases        :: [Text],
                     labelRelationLists  :: [RelationList],
                     labelTags           :: [Tag],
                     labelRating         :: Maybe Rating,
                     labelUserRating     :: Maybe UserRating } deriving (Show, Eq)

data Recording = Recording { recordingId             :: Text,
                             recordingTitle          :: Text,
                             recordingLength         :: Maybe Int,
                             recordingDisambiguation :: Maybe Text,
                             recordingArtistCredit   :: [NameCredit],
                             recordingReleases       :: [Release],
                             recordingIdentifiers    :: [Identifier],
                             recordingRelationLists  :: [RelationList],
                             recordingTags           :: [Tag],
                             recordingRating         :: Maybe Rating,
                             recordingUserRating     :: Maybe UserRating } deriving (Show, Eq)

data Release = Release { releaseId                 :: Text,
                         releaseTitle              :: Text,
                         releaseStatus             :: Maybe Text,
                         releaseQuality            :: Maybe Quality,
                         releaseDisambiguation     :: Maybe Text,
                         releasePackaging          :: Maybe Text,
                         releaseTextRepresentation :: Maybe TextRepresentation, -- language, script
                         releaseArtistCredit       :: [NameCredit],
                         releaseReleaseGroup       :: Maybe ReleaseGroup,
                         releaseDate               :: Maybe PartialDate,
                         releaseCountry            :: Maybe CountryCode,
                         releaseBarcode            :: Maybe Text,
                         releaseASIN               :: Maybe ASIN,
                         --TODO: label info
                         --TODO: medium info
                         relationLists             :: [RelationList] } deriving (Show, Eq)

data ReleaseGroup = ReleaseGroup { releaseGroupId               :: Text,
                                   releaseGroupType             :: Maybe Text,
                                   releaseGroupTitle            :: Text,
                                   releaseGroupDisambiguation   :: Maybe Text,
                                   releaseGroupComment          :: Maybe Text,
                                   releaseGroupFirstReleaseDate :: Maybe PartialDate,
                                   releaseGroupArtistCredit     :: [NameCredit],
                                   releaseGroupReleases         :: [Release],
                                   releaseGroupRelationLists    :: [RelationList],
                                   releaseGroupTags             :: [Tag],
                                   releaseGroupRating           :: Maybe Rating,
                                   releaseGroupUserRating       :: Maybe UserRating } deriving (Show, Eq)

data Work = Work { workId             :: Text,
                   workType           :: Maybe Text,
                   workTitle          :: Text,
                   workArtistCredit   :: [NameCredit],
                   workISWC           :: Maybe Identifier,
                   workDisambiguation :: Maybe Text,
                   workAliases        :: [Text],
                   workRelationLists  :: [RelationList],
                   workTags           :: [Tag],
                   workRating         :: Maybe Rating,
                   workUserRating     :: Maybe Rating } deriving (Show, Eq)

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

data Direction = Both | Forward | Backward deriving (Show, Eq)

type RelationList = [Relation]

data Relation = ArtistRelation { relationType      :: Text,
                                 relationTarget    :: MBID,
                                 relationLifeSpan  :: LifeSpan,
                                 relationDirection :: Maybe Direction,
                                 relationArtist    :: Artist } |
                ReleaseRelation { relationType      :: Text,
                                  relationTarget    :: MBID,
                                  relationLifeSpan  :: LifeSpan,
                                  relationDirection :: Maybe Direction,
                                  relationRelease   :: Release } |
                ReleaseGroupRelation { relationType         :: Text,
                                       relationTarget       :: MBID,
                                       relationLifeSpan     :: LifeSpan,
                                       relationDirection    :: Maybe Direction,
                                       relationReleaseGroup :: ReleaseGroup } |
                RecordingRelation { relationType      :: Text,
                                    relationTarget    :: MBID,
                                    relationLifeSpan  :: LifeSpan,
                                    relationDirection :: Maybe Direction,
                                    relationRecording :: Recording } |
                LabelRelation { relationType      :: Text,
                                relationTarget    :: MBID,
                                relationLifeSpan  :: LifeSpan,
                                relationDirection :: Maybe Direction,
                                relationLabel     :: Label } |
                WorkRelation { relationType      :: Text,
                               relationTarget    :: MBID,
                               relationLifeSpan  :: LifeSpan,
                               relationDirection :: Maybe Direction,
                               relationWork      :: Work } deriving (Show, Eq) --TODO: verify artist is guaranteeed

data Tag = Tag { tagName :: Text } | 
           UserTag { tagName :: Text } deriving (Show, Eq)

data Collection = Collection

data Identifier = DiscID Text | 
                  PUID Text |
                  ISRC Text |
                  ISWC Text deriving (Show, Eq)
data MBID = MBID Text deriving (Show, Eq)

data ASIN = ASIN Text deriving (Show, Eq)
