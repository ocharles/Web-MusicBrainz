{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.MusicBrainz
    ( -- * Looking Up MusicBrainz Entities
      getArtistByMBID
    , getLabelByMBID
    , getRecordingByMBID
    , getReleaseByMBID
    , getReleaseGroupByMBID
    , getWorkByMBID
    , runMusicBrainz

      -- * Possible inc flags
    , incArtists, incLabels, incRecordings, incReleases, incReleaseGroups, incWorks
    , incArtistRelationships, incLabelRelationships, incRecordingRelationships
    , incReleaseRelationships, incReleaseGroupRelationships, incURLRelationships
    , incWorkRelationships, incRecordingLevelRelationships, incWorkLevelRelationships
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (transResourceT)
import qualified Data.ByteString.Char8 as BS
import Data.Conduit (($$), ResourceT, ResourceIO, runResourceT)
import Data.List (intercalate)
import Data.Text (Text)
import Data.UUID (UUID, toString)
import Network.HTTP.Conduit (Manager, http, responseBody, withManager)
import qualified Network.HTTP.Conduit as H
import Text.XML (sinkDoc)
import qualified Text.XML as XML
import Text.XML.Cursor (fromDocument)

import Text.XML.Cursor.FromXML
import Web.MusicBrainz.Types
import Web.MusicBrainz.XML ()

data ArtistFetchInc =
    ArtistRecordings | ArtistReleases | ArtistReleaseGroups | ArtistWorks
  | ArtistArtistRelationships | ArtistLabelRelationships
  | ArtistRecordingRelationships | ArtistReleaseRelationships
  | ArtistReleaseGroupRelationships | ArtistURLRelationships
  | ArtistWorkRelationships

data CollectionFetchInc = CollectionReleases

data LabelFetchInc =
    LabelReleases
  | LabelArtistRelationships | LabelLabelRelationships
  | LabelRecordingRelationships | LabelReleaseRelationships
  | LabelReleaseGroupRelationships | LabelURLRelationships
  | LabelWorkRelationships

data RecordingFetchInc =
    RecordingArtists | RecordingReleases
  | RecordingArtistRelationships | RecordingLabelRelationships
  | RecordingRecordingRelationships | RecordingReleaseRelationships
  | RecordingReleaseGroupRelationships | RecordingURLRelationships
  | RecordingWorkRelationships

data ReleaseFetchInc =
    ReleaseArtists | ReleaseLabels | ReleaseRecordings | ReleaseReleaseGroups
  | ReleaseRecordingLevelRels | ReleaseWorkLevelRels
  | ReleaseArtistRelationships | ReleaseLabelRelationships
  | ReleaseRecordingRelationships | ReleaseReleaseRelationships
  | ReleaseReleaseGroupRelationships | ReleaseURLRelationships
  | ReleaseWorkRelationships

data ReleaseGroupFetchInc =
    ReleaseGroupArtists | ReleaseGroupReleases
  | ReleaseGroupArtistRelationships | ReleaseGroupLabelRelationships
  | ReleaseGroupRecordingRelationships | ReleaseGroupReleaseRelationships
  | ReleaseGroupReleaseGroupRelationships | ReleaseGroupURLRelationships
  | ReleaseGroupWorkRelationships

data WorkFetchInc =
    WorkArtistRelationships | WorkLabelRelationships
  | WorkRecordingRelationships | WorkReleaseRelationships
  | WorkReleaseGroupRelationships | WorkURLRelationships
  | WorkWorkRelationships

instance IncFlag ArtistFetchInc where
  incFlag ArtistRecordings = "recordings"
  incFlag ArtistReleases = "releases"
  incFlag ArtistReleaseGroups = "release-groups"
  incFlag ArtistWorks = "works"
  incFlag ArtistArtistRelationships = "artist-rels"
  incFlag ArtistLabelRelationships = "label-rels"
  incFlag ArtistRecordingRelationships = "recording-rels"
  incFlag ArtistReleaseRelationships = "release-rels"
  incFlag ArtistReleaseGroupRelationships = "release-group-rels"
  incFlag ArtistURLRelationships = "url-rels"
  incFlag ArtistWorkRelationships = "work-rels"

instance IncFlag LabelFetchInc where
  incFlag LabelReleases = "releases"
  incFlag LabelArtistRelationships = "artist-rels"
  incFlag LabelLabelRelationships = "label-rels"
  incFlag LabelRecordingRelationships = "recording-rels"
  incFlag LabelReleaseRelationships = "release-rels"
  incFlag LabelReleaseGroupRelationships = "release-group-rels"
  incFlag LabelURLRelationships = "url-rels"
  incFlag LabelWorkRelationships = "work-rels"

instance IncFlag RecordingFetchInc where
  incFlag RecordingArtists = "artists"
  incFlag RecordingReleases = "releases"
  incFlag RecordingArtistRelationships = "artist-rels"
  incFlag RecordingLabelRelationships = "label-rels"
  incFlag RecordingRecordingRelationships = "recording-rels"
  incFlag RecordingReleaseRelationships = "release-rels"
  incFlag RecordingReleaseGroupRelationships = "release-group-rels"
  incFlag RecordingURLRelationships = "url-rels"
  incFlag RecordingWorkRelationships = "work-rels"

instance IncFlag ReleaseFetchInc where
  incFlag ReleaseArtists = "artists"
  incFlag ReleaseLabels = "labels"
  incFlag ReleaseRecordings = "recordings"
  incFlag ReleaseReleaseGroups = "release-groups"
  incFlag ReleaseRecordingLevelRels = "recording-level-rels"
  incFlag ReleaseWorkLevelRels = "work-level-rels"
  incFlag ReleaseArtistRelationships = "artist-rels"
  incFlag ReleaseLabelRelationships = "label-rels"
  incFlag ReleaseRecordingRelationships = "recording-rels"
  incFlag ReleaseReleaseRelationships = "release-rels"
  incFlag ReleaseReleaseGroupRelationships = "release-group-rels"
  incFlag ReleaseURLRelationships = "url-rels"
  incFlag ReleaseWorkRelationships = "work-rels"

instance IncFlag ReleaseGroupFetchInc where
  incFlag ReleaseGroupArtists = "artists"
  incFlag ReleaseGroupReleases = "releases"
  incFlag ReleaseGroupArtistRelationships = "artist-rels"
  incFlag ReleaseGroupLabelRelationships = "label-rels"
  incFlag ReleaseGroupRecordingRelationships = "recording-rels"
  incFlag ReleaseGroupReleaseRelationships = "release-rels"
  incFlag ReleaseGroupReleaseGroupRelationships = "release-group-rels"
  incFlag ReleaseGroupURLRelationships = "url-rels"
  incFlag ReleaseGroupWorkRelationships = "work-rels"

instance IncFlag WorkFetchInc where
  incFlag _ = error "Do not know how to turn a work inc flag into text"

class IncArtists flag where incArtists :: flag
class IncLabels flag where incLabels :: flag
class IncRecordings flag where incRecordings :: flag
class IncReleases flag where incReleases :: flag
class IncReleaseGroups flag where incReleaseGroups :: flag
class IncWorks flag where incWorks :: flag
class IncRelationships flag where
  incArtistRelationships :: flag
  incLabelRelationships :: flag
  incRecordingRelationships :: flag
  incReleaseRelationships :: flag
  incReleaseGroupRelationships :: flag
  incURLRelationships :: flag
  incWorkRelationships :: flag

incRecordingLevelRelationships = ReleaseRecordingLevelRels
incWorkLevelRelationships = ReleaseWorkLevelRels

class IncFlag flag where incFlag :: flag -> String

instance IncRecordings ArtistFetchInc where incRecordings = ArtistRecordings
instance IncReleases ArtistFetchInc where incReleases = ArtistReleases
instance IncReleaseGroups ArtistFetchInc where incReleaseGroups = ArtistReleaseGroups
instance IncWorks ArtistFetchInc where incWorks = ArtistWorks
instance IncRelationships ArtistFetchInc where
  incArtistRelationships = ArtistArtistRelationships
  incLabelRelationships = ArtistLabelRelationships
  incRecordingRelationships = ArtistRecordingRelationships
  incReleaseRelationships = ArtistReleaseRelationships
  incReleaseGroupRelationships = ArtistReleaseGroupRelationships
  incURLRelationships = ArtistURLRelationships
  incWorkRelationships = ArtistWorkRelationships

instance IncReleases LabelFetchInc where incReleases = LabelReleases
instance IncRelationships LabelFetchInc where
  incArtistRelationships = LabelArtistRelationships
  incLabelRelationships = LabelLabelRelationships
  incRecordingRelationships = LabelRecordingRelationships
  incReleaseRelationships = LabelReleaseRelationships
  incReleaseGroupRelationships = LabelReleaseGroupRelationships
  incURLRelationships = LabelURLRelationships
  incWorkRelationships = LabelWorkRelationships

instance IncReleases CollectionFetchInc where incReleases = CollectionReleases

instance IncArtists RecordingFetchInc where incArtists = RecordingArtists
instance IncReleases RecordingFetchInc where incReleases = RecordingReleases
instance IncRelationships RecordingFetchInc where
  incArtistRelationships = RecordingArtistRelationships
  incLabelRelationships = RecordingLabelRelationships
  incRecordingRelationships = RecordingRecordingRelationships
  incReleaseRelationships = RecordingReleaseRelationships
  incReleaseGroupRelationships = RecordingReleaseGroupRelationships
  incURLRelationships = RecordingURLRelationships
  incWorkRelationships = RecordingWorkRelationships


instance IncArtists ReleaseFetchInc where incArtists  = ReleaseArtists
instance IncLabels ReleaseFetchInc where incLabels = ReleaseLabels
instance IncRecordings ReleaseFetchInc where incRecordings = ReleaseRecordings
instance IncReleaseGroups ReleaseFetchInc where incReleaseGroups = ReleaseReleaseGroups
instance IncRelationships ReleaseFetchInc where
  incArtistRelationships = ReleaseArtistRelationships
  incLabelRelationships = ReleaseLabelRelationships
  incRecordingRelationships = ReleaseRecordingRelationships
  incReleaseRelationships = ReleaseReleaseRelationships
  incReleaseGroupRelationships = ReleaseReleaseGroupRelationships
  incURLRelationships = ReleaseURLRelationships
  incWorkRelationships = ReleaseWorkRelationships

instance IncArtists ReleaseGroupFetchInc where incArtists = ReleaseGroupArtists
instance IncReleases ReleaseGroupFetchInc where incReleases = ReleaseGroupReleases
instance IncRelationships ReleaseGroupFetchInc where
  incArtistRelationships = ReleaseGroupArtistRelationships
  incLabelRelationships = ReleaseGroupLabelRelationships
  incRecordingRelationships = ReleaseGroupRecordingRelationships
  incReleaseRelationships = ReleaseGroupReleaseRelationships
  incReleaseGroupRelationships = ReleaseGroupReleaseGroupRelationships
  incURLRelationships = ReleaseGroupURLRelationships
  incWorkRelationships = ReleaseGroupWorkRelationships

instance IncRelationships WorkFetchInc where
  incArtistRelationships = WorkArtistRelationships
  incLabelRelationships = WorkLabelRelationships
  incRecordingRelationships = WorkRecordingRelationships
  incReleaseRelationships = WorkReleaseRelationships
  incReleaseGroupRelationships = WorkReleaseGroupRelationships
  incURLRelationships = WorkURLRelationships
  incWorkRelationships = WorkWorkRelationships

getArtistByMBID :: UUID -> [ArtistFetchInc] -> ResourceT (ReaderT Manager IO) Artist
getArtistByMBID mbid inc =
  runAndParse ("/ws/2/artist/" ++ (toString mbid)) inc [ "artist" ]

getLabelByMBID :: UUID -> [LabelFetchInc] -> ResourceT (ReaderT Manager IO) Label
getLabelByMBID mbid inc =
  runAndParse ("/ws/2/label/" ++ toString mbid) inc [ "label" ]

getRecordingByMBID :: UUID -> [RecordingFetchInc] -> ResourceT (ReaderT Manager IO) Recording
getRecordingByMBID mbid inc =
  runAndParse ("/ws/2/recording/" ++ toString mbid) inc [ "recording" ]

getReleaseByMBID :: UUID -> [ReleaseFetchInc] -> ResourceT (ReaderT Manager IO) Release
getReleaseByMBID mbid inc =
  runAndParse ("/ws/2/release/" ++ toString mbid) inc [ "release" ]

getReleaseGroupByMBID :: UUID -> [ReleaseGroupFetchInc] -> ResourceT (ReaderT Manager IO) ReleaseGroup
getReleaseGroupByMBID mbid inc =
  runAndParse ("/ws/2/release-group/" ++ toString mbid) inc [ "release-group" ]

getWorkByMBID :: UUID -> [WorkFetchInc] -> ResourceT (ReaderT Manager IO) Work
getWorkByMBID mbid inc =
  runAndParse ("/ws/2/work/" ++ toString mbid) inc [ "work" ]

runAndParse :: (FromXML a, IncFlag f)
            => String -> [f] -> [Text] -> ResourceT (ReaderT Manager IO) a
runAndParse resourcePath incFlags docPath = do
  let req = H.def { H.path = BS.pack resourcePath
                  , H.host = "musicbrainz.org"
                  , H.queryString = incString}
  res <- lift ask >>= http req
  document <- responseBody res $$ sinkDoc XML.def
  liftIO $ fromDocument document !<//=> docPath
  where incString = BS.pack $ "inc=" ++ intercalate "+" (map incFlag incFlags)

runMusicBrainz :: ResourceIO m => ResourceT (ReaderT Manager m) a -> m a
runMusicBrainz actions = withManager $ \m -> transResourceT (\a -> runReaderT a m) actions
