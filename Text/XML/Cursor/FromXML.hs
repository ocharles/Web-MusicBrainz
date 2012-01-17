{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.XML.Cursor.FromXML
    ( FromXML
    , fromXML
    , (!<@>)
    , (?<@>)
    , (!<|>)
    , cont
    , (?<|>)
    , (?<.>)
    , (!<.>)
    , (!<=>)
    , (?<=>)
    , (<//.>)
    , (!<//.>)
    , (<//=>)
    , (!<//=>)
    , (!<//|>)
    , XmlException, xmlErrorMessage
    ) where

import Control.Applicative (Applicative)
import Control.Exception (Exception)
import Control.Failure (Failure)
import Control.Monad ((<=<))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Text.XML.Cursor ( Cursor, ($|), (&/), ($/), laxAttribute, laxElement
                       , content, force )

-- | FromXML inspired by Aeson's FromJSON typeclass.
class FromXML a where
  fromXML :: (Functor m, Applicative m, Failure XmlException m) => Cursor -> m a

-- | Get the value of an attribute at the cursor, and throw an exception if the
-- attribute does not exist.
(!<@>) :: Failure XmlException m => Cursor -> Text -> m Text
el !<@> n = forceEx ("missing " ++ T.unpack n) $ el $| laxAttribute n

-- | Attempt to get the value of an attribute at the cursor, returning @Nothing@
-- if the attribute does not exist.
(?<@>) :: Cursor -> Text -> Maybe Text
el ?<@> n =  listToMaybe $ el $| laxAttribute n

-- | Get the text content of an element, throwing an exception if the element
-- does not exist.
(!<|>) :: Failure XmlException m => Cursor -> Text -> m Text
el !<|> n = forceEx ("missing " ++ T.unpack n) $ el $/ (cont <=< laxElement n)

-- | Get text content from the current point of the cursor, throwing an
-- exception if there is no content.
cont :: Failure XmlException m => Cursor -> m Text
cont el = forceEx "no content" $ el $/ content

-- | Get content from an element, returning @Nothing@ if the element cannot be
-- found.
(?<|>) :: Cursor -> Text -> Maybe Text
el ?<|> n = listToMaybe $ el $/ (laxElement n &/ content)

-- | Attempt to move into an element, returning @Nothing@ if the element does
-- not exist.
(?<.>) :: Cursor -> Text -> Maybe Cursor
el ?<.> n = listToMaybe $ el $/ laxElement n

-- | Move the cursor into an element, throwing an exception if the element does
-- not exist.
(!<.>) :: (Failure XmlException m) => Cursor -> Text -> m Cursor
el !<.> n = forceEx ("missing " ++ T.unpack n) $ el $/ laxElement n

-- | Move the cursor into an element, and use the current cursor context to
-- deserialize XML into an "a". Throw an exception if the element does not
-- exist.
(!<=>) :: (Failure XmlException m, Applicative m, FromXML a)
       => Cursor -> Text -> m a
el !<=> n = fromXML =<< el !<.> n

-- | Attempt to move the cursor into an element, and deserialize it into an
-- @a@. If the element does not exist, @Nothing@ will be returned.
(?<=>) :: (FromXML a) => Cursor -> Text -> Maybe a
el ?<=> n = fromXML =<< el ?<.> n

-- REFACTOR: ! is inaccurate. It returns an array so there is no opportunity for
-- failure. Find leaf nodes given list of paths to walk down
(<//.>) :: Cursor -> [Text] -> [Cursor]
el <//.> path = el $/ (foldl1 (&/) $ map laxElement path)

(!<//.>) :: (Functor m, Applicative m, Failure XmlException m)
         => Cursor -> [Text] -> m Cursor
el !<//.> path =
    forceEx ("missing " ++ pathStr) $ el $/ (foldl1 (&/) $ map laxElement path)
  where pathStr = T.unpack . T.concat $ path

(<//=>) :: (FromXML a, Functor m, Applicative m, Failure XmlException m)
        => Cursor -> [Text] -> m [a]
el <//=> path = mapM fromXML $ el <//.> path

(!<//=>) :: (FromXML a, Functor m, Applicative m, Failure XmlException m)
         => Cursor -> [Text] -> m a
el !<//=> path = fromXML =<< el !<//.> path

(!<//|>) :: Cursor -> [Text] -> [Text]
el !<//|> path = concatMap ($/ content) $ el <//.> path

forceEx :: Failure XmlException m => String -> [a] -> m a
forceEx = force . XmlException

newtype XmlException = XmlException { xmlErrorMessage :: String }
    deriving (Show, Typeable)

instance Exception XmlException
