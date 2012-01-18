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
    , ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (transResourceT)
import qualified Data.ByteString.Char8 as BS
import Data.Conduit (($$), ResourceT, ResourceIO, runResourceT)
import Data.List (intercalate)
import Data.Text (Text)
import Data.UUID (UUID, toString)
import Network.HTTP.Conduit (Manager, http, responseBody, newManager)
import qualified Network.HTTP.Conduit as H
import Text.XML (sinkDoc)
import qualified Text.XML as XML
import Text.XML.Cursor (fromDocument)

import Text.XML.Cursor.FromXML
import Web.MusicBrainz.Types
import Web.MusicBrainz.XML ()

data ArtistFetchInc = ArtistRecordings | ArtistReleases | ArtistReleaseGroups | ArtistWorks
data CollectionFetchInc = CollectionReleases
data LabelFetchInc = LabelReleases
data RecordingFetchInc = RecordingArtists | RecordingReleases
data ReleaseFetchInc = ReleaseArtists | ReleaseLabels | ReleaseRecordings | ReleaseReleaseGroups
data ReleaseGroupFetchInc = ReleaseGroupArtists | ReleaseGroupReleases
data WorkFetchInc

instance IncFlag ArtistFetchInc where
  incFlag ArtistRecordings = "recordings"
  incFlag ArtistReleases = "releases"
  incFlag ArtistReleaseGroups = "release-groups"
  incFlag ArtistWorks = "works"

instance IncFlag LabelFetchInc where
  incFlag LabelReleases = "releases"

instance IncFlag RecordingFetchInc where
  incFlag RecordingArtists = "artists"
  incFlag RecordingReleases = "releases"

instance IncFlag ReleaseFetchInc where
  incFlag ReleaseArtists = "artists"
  incFlag ReleaseLabels = "labels"
  incFlag ReleaseRecordings = "recordings"
  incFlag ReleaseReleaseGroups = "release-groups"

instance IncFlag ReleaseGroupFetchInc where
  incFlag ReleaseGroupArtists = "artists"
  incFlag ReleaseGroupReleases = "releases"

instance IncFlag WorkFetchInc where
  incFlag _ = error "Do not know how to turn a work inc flag into text"

class IncArtists flag where incArtists :: flag
class IncLabels flag where incLabels :: flag
class IncRecordings flag where incRecordings :: flag
class IncReleases flag where incReleases :: flag
class IncReleaseGroups flag where incReleaseGroups :: flag
class IncWorks flag where incWorks :: flag

class IncFlag flag where incFlag :: flag -> String

instance IncRecordings ArtistFetchInc where incRecordings = ArtistRecordings
instance IncReleases ArtistFetchInc where incReleases = ArtistReleases
instance IncReleaseGroups ArtistFetchInc where incReleaseGroups = ArtistReleaseGroups
instance IncWorks ArtistFetchInc where incWorks = ArtistWorks

instance IncReleases LabelFetchInc where incReleases = LabelReleases

instance IncReleases CollectionFetchInc where incReleases = CollectionReleases

instance IncArtists RecordingFetchInc where incArtists = RecordingArtists
instance IncReleases RecordingFetchInc where incReleases = RecordingReleases

instance IncArtists ReleaseFetchInc where incArtists  = ReleaseArtists
instance IncLabels ReleaseFetchInc where incLabels = ReleaseLabels
instance IncRecordings ReleaseFetchInc where incRecordings = ReleaseRecordings
instance IncReleaseGroups ReleaseFetchInc where incReleaseGroups = ReleaseReleaseGroups

instance IncArtists ReleaseGroupFetchInc where incArtists = ReleaseGroupArtists
instance IncReleases ReleaseGroupFetchInc where incReleases = ReleaseGroupReleases

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
runMusicBrainz actions = runResourceT $ do
  manager <- newManager
  transResourceT (runWithManager manager) actions
  where runWithManager manager a = runReaderT a manager
