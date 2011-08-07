--------------------------------------------------------------------
-- |
-- Module      : Audio.MusicBrainz.Monad
-- Description : Monadic interface for communcating with the MusicBrainz API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Audio.MusicBrainz.Monad (MusicBrainzEnv(..), MusicBrainzM(..)) where

import Data.Text (Text)
import Control.Monad.Reader

-- |Authentication environment used in MusicBrainz API calls
data MusicBrainzEnv = MusicBrainzEnv { mbClient   :: Text, -- ^ Name of the client software being used in the form of softwarename-version
                                       mbUsername :: Maybe Text, -- ^ Username required only for POSTing data
                                       mbPassword :: Maybe Text  -- ^ Password required only for POSTing data
                                      }

-- |IO wrapper used to chain and compose MusicBrainz API actions
newtype MusicBrainzM a = MusicBrainzM {unMusicBrainzM :: ReaderT MusicBrainzEnv IO a} 
  deriving (Monad, MonadIO, MonadReader MusicBrainzEnv)
