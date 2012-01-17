{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.MusicBrainz
    ( getArtistByMBID
    , getLabelByMBID
    , runMusicBrainz) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (transResourceT)
import qualified Data.ByteString.Char8 as BS
import Data.Conduit (($$), ResourceT, ResourceIO, runResourceT)
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

getArtistByMBID :: UUID -> ResourceT (ReaderT Manager IO) Artist
getArtistByMBID mbid =
  runAndParse ("/ws/2/artist/" ++ toString mbid) [ "artist" ]

getLabelByMBID :: UUID -> ResourceT (ReaderT Manager IO) Label
getLabelByMBID mbid =
  runAndParse ("/ws/2/label/" ++ toString mbid) [ "label" ]

runAndParse :: FromXML a => String -> [Text] -> ResourceT (ReaderT Manager IO) a
runAndParse resourcePath docPath = do
  let req = H.def { H.path = BS.pack resourcePath
                  , H.host = "musicbrainz.org" }
  res <- lift ask >>= http req
  document <- responseBody res $$ sinkDoc XML.def
  liftIO $ fromDocument document !<//=> docPath

runMusicBrainz :: ResourceIO m => ResourceT (ReaderT Manager m) a -> m a
runMusicBrainz actions = runResourceT $ do
  manager <- newManager
  transResourceT (runWithManager manager) actions
  where runWithManager manager a = runReaderT a manager
