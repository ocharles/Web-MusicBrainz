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
                                Tag(..),
                                LifeSpan
                                ) where

import qualified Data.Text as T
import Data.ISO3166_CountryCodes

type LifeSpan = (Maybe PartialDate, Maybe PartialDate)
data PartialDate = PartialDate Int (Maybe Int) (Maybe Int) deriving (Show, Eq)

---- Core Resources
-- the rng/talking to the devs revealed that artists, recordings, releases, release-groups, works can only be guaranteed to have a name/title and id, everything else is optional and cannot be relied upon
--TODO: most of these should be mabe
data Artist = Artist { artistId             :: T.Text,
                       artistType           :: T.Text,
                       artistName           :: T.Text,
                       artistAliases        :: [T.Text],
                       artistSortName       :: Maybe T.Text,
                       artistLifeSpan       :: LifeSpan,
                       artistCountry        :: Maybe CountryCode,
                       artistGender         :: Maybe T.Text,
                       artistDisambiguation :: Maybe T.Text,
                       artistRecordings     :: [Recording],
                       artistReleases       :: [Release],
                       artistLabels         :: [Label],
                       artistWorks          :: [Work],
                       artistRelationLists  :: [[Relation]], -- Iffy
                       artistUserTags       :: [UserTag],
                       artistRating         :: Maybe Rating,
                       artistUserRating     :: Maybe UserRating,
                       artistTags           :: [Tag] } deriving (Show, Eq)

data Label = Label deriving (Show, Eq)

data Recording = Recording deriving (Show, Eq)

data Release = Release deriving (Show, Eq)

data ReleaseGroup = ReleaseGroup deriving (Show, Eq)

data Work = Work deriving (Show, Eq)

---- Non-Core Resources
-- Ratings are 1-5
data Rating = Rating { ratingVotes :: Int, 
                       ratingScore :: Float} deriving (Show, Eq)

data UserRating = UserRating { userRatingVotes :: Int, 
                               userRatingScore :: Float} deriving (Show, Eq)

data Relation = Relation deriving (Show, Eq)

data UserTag = UserTag deriving (Show, Eq)

newtype Tag = Tag { tagName :: T.Text } deriving (Show, Eq)

data Collection = Collection

data Identifier = DiscID T.Text | 
                  PUID T.Text |
                  ISRC T.Text |
                  ISWC T.Text
