--------------------------------------------------------------------
-- |
-- Module      : Audio.MusicBrainz
-- Description : Toplevel module for the MusicBrainz API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
-- Toplevel module for the MusicBrainz API.
-- 
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Audio.MusicBrainz (version, getArtistByMBID) where

import           Audio.MusicBrainz.Types
import           Audio.MusicBrainz.Monad
import           Audio.MusicBrainz.XML
import           Audio.MusicBrainz.XML.FromXML (FromXML, XmlException)

import           Control.Monad.Reader
import           Control.Applicative (Applicative)
import qualified Control.Failure as F
import           Data.ByteString (ByteString)
import           Data.Text (Text, append, pack)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Network.HTTP.Enumerator
import           Network.HTTP.Types (methodGet,
                                     Header(..),
                                     headerContentType,
                                     Method,
                                     Query)
import           Network.TLS (TLSCertificateUsage(CertificateUsageAccept))
import           Text.XML.Enumerator.Parse (parseLBS_)

version :: Text
version = "0.0.0"

-- TODO: handle not found specially
getArtistByMBID :: MBID -> MusicBrainzM (Maybe Artist)
getArtistByMBID (MBID mbid) = withReadEnv $ \client -> do
  resp <- doGet client pth []
  maybeParse resp ["metadata", "artist"]
  where pth = "/artist/" `append` mbid

withReadEnv :: (Text -> MusicBrainzM a) -> MusicBrainzM a
withReadEnv fn = asks mbClient >>= fn

doGet :: Text -> Text -> Query -> MusicBrainzM (Int, LBS.ByteString)
doGet client pth params = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
  where req = genReadRequest client pth params methodGet LBS.empty


maybeParse :: (Functor m, 
               Applicative m,
               F.Failure XmlException m,
               FromXML a)
               => (Int, LBS.ByteString)
               -> [Text]
               -> m (Maybe a)
maybeParse (404, _) _ = return Nothing
maybeParse (_, body) nodes = fromXMLLBS body nodes

-- TODO: curry this
genWriteRequest :: Text -> Text -> Text -> Text -> Query -> Method -> LBS.ByteString -> Request IO
genWriteRequest user pass client pth params meth pay = applyBasicAuth u p $ genReadRequest client pth params meth pay
  where u = encodeUtf8 user
        p = encodeUtf8 pass

genReadRequest :: Text -> Text -> Query -> Method -> LBS.ByteString -> Request IO
genReadRequest client pth params meth pay = Request { 
  method         = meth,
  secure         = True,
  checkCerts     = \_ -> return CertificateUsageAccept, -- uhhh
  host           = h,
  port           = 443,
  path           = padPath pth,
  proxy          = Nothing,
  rawBody        = False,
  queryString    = params,
  requestHeaders = headers,
  requestBody    = RequestBodyLBS pay }
  where h       = encodeUtf8 "musicbrainz.org"
        headers = [headerContentType "application/json", userAgentHeader]

padPath :: Text -> ByteString
padPath pth = encodeUtf8 $ "/ws/2" `append` pth

userAgent :: ByteString
userAgent = encodeUtf8 $ "Audio.MusicBrainz " `append` version

userAgentHeader :: Header
userAgentHeader = ("User-Agent", userAgent)
