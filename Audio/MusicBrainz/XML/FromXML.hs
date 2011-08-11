-- TODO: will probably be split off into a different package. (mostly) Infix
-- operators for traversing a document with cursors and converting XML into
-- arbitrary datatypes
{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable #-}
module Audio.MusicBrainz.XML.FromXML (FromXML, 
                                      fromXML,
                                      (!<@>),
                                      (?<@>),
                                      (!<|>),
                                      cont,
                                      (?<|>),
                                      (?<.>),
                                      (!<.>),
                                      (!<=>),
                                      (?<=>),
                                      (<//.>),
                                      (!<//.>),
                                      (<//=>),
                                      (!<//=>),
                                      (!<//|>),
                                      XmlException
                                      ) where

import           Control.Applicative
import           Control.Exception (Exception)
import           Control.Failure (Failure)
import           Control.Monad
import           Data.Maybe (listToMaybe)
import           Data.Monoid
import           Data.Text (Text, unpack, append)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Text.XML.Enumerator.Cursor
import           Text.XML.Enumerator.Resolved

-- FromXML inspired by Aeson's FromJSON typeclass
class FromXML a where
  fromXML :: (Functor m, Applicative m, Failure XmlException m) => Cursor -> m a

-- Get attribute of cursor
(!<@>) :: Failure XmlException m => Cursor -> Text -> m Text
el !<@> n = forceEx ("missing " ++ unpack n) $ el $| laxAttribute n

(?<@>) :: Cursor -> Text -> Maybe Text
el ?<@> n =  listToMaybe $ el $| laxAttribute n

-- Shorthand to get content of named tag
(!<|>) :: Failure XmlException m => Cursor -> Text -> m Text
el !<|> n = forceEx ("missing " ++ unpack n) $ el $/ (cont <=< laxElement n)

-- Get content from the current element (custom unary not allowed)
cont :: Failure XmlException m => Cursor -> m Text
cont el = forceEx "no content" $ el $/ content

-- (Maybe) get content
(?<|>) :: Cursor -> Text -> Maybe Text
el ?<|> n = listToMaybe $ el $/ (laxElement n &/ content)

-- (Maybe) find a node
(?<.>) :: Cursor -> Text -> Maybe Cursor
el ?<.> n = listToMaybe $ el $/ laxElement n

(!<.>) :: (Failure XmlException m) => Cursor -> Text -> m Cursor
el !<.> n = forceEx ("missing " ++ unpack n) $ el $/ laxElement n

(!<=>) :: (Failure XmlException m, Applicative m, FromXML a)
          => Cursor 
          -> Text 
          -> m a
el !<=> n = fromXML =<< el !<.> n

(?<=>) :: (FromXML a) => Cursor -> Text -> Maybe a
el ?<=> n = fromXML =<< el ?<.> n

--REFACTOR: ! is inaccurate. It returns an array so there is no opportunity for failure
-- Find leaf nodes given list of paths to walk down
(<//.>) :: Cursor -> [Text] -> [Cursor]
el <//.> path = el $/ (foldl1 (&/) $ map laxElement path)

(!<//.>) :: (Functor m, Applicative m, Failure XmlException m) => Cursor -> [Text] -> m Cursor
el !<//.> path = forceEx ("missing " ++ pathStr) $ el $/ (foldl1 (&/) $ map laxElement path)
  where pathStr = T.unpack . T.concat $ path

(<//=>) :: (FromXML a, Functor m, Applicative m, Failure XmlException m) => Cursor -> [Text] -> m [a]
el <//=> path = mapM fromXML $ el <//.> path

(!<//=>) :: (FromXML a, Functor m, Applicative m, Failure XmlException m) => Cursor -> [Text] -> m a
el !<//=> path = fromXML =<< el !<//.> path

(!<//|>) :: Cursor -> [Text] -> [Text]
el !<//|> path = concatMap ($/ content) $ el <//.> path

forceEx :: Failure XmlException m => String -> [a] -> m a
forceEx = force . XmlException

newtype XmlException = XmlException { xmlErrorMessage :: String }
    deriving (Show, Typeable)

instance Exception XmlException

