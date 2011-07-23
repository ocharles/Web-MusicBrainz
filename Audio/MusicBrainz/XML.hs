{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, TypeSynonymInstances #-}
module Audio.MusicBrainz.XML (fromXML,
                              (<//.>)
                              ) where

import           Data.List.Split (splitOn)
import           Data.Maybe (listToMaybe)
import           Data.Typeable
import           Data.XML.Types
import qualified Control.Failure            as F
import           Text.XML.Enumerator.Resolved (Name(..))
import qualified Data.Text as T
import           Text.XML.Enumerator.Cursor
import qualified Text.XML.Enumerator.Cursor as Cu
import           Text.XML.Enumerator.Resolved
import qualified Control.Exception            as C

import Control.Applicative
import Control.Monad
import Data.Monoid

import Audio.MusicBrainz.Types

-- FromXML inspired by Aeson's FromJSON typeclass
class FromXML a where
  fromXML :: (Functor m, Applicative m,  F.Failure XmlException m) => Cu.Cursor -> m a

---- Instances

instance FromXML Artist where
  fromXML el = Artist <$> el !<@> "id"
                      <*> el !<@> "type"
                      <*> el !<|> "name"
                      <*> (pure $ el !<//|> ["alias-list", "alias"])
                      <*> (pure $                       el ?<|> "sort-name")
                      <*> (parseLifeSpan $              el ?<.> "life-span")
                      <*> (pure $ (read . T.unpack) <$> el ?<|> "country")
                      <*> (pure $                       el ?<|> "gender")
                      <*> (pure $                       el ?<|> "disambiguation")
                      --TODO
                      <*> el <//=> ["recording-list", "recording"]
                      <*> el <//=> ["release-list", "release"]
                      <*> el <//=> ["label-list", "label"]
                      <*> el <//=> ["work-list", "work"]
                      <*> el <//=> ["relation-list"]

                      <*> (pure $ parseRating =<<       el ?<.> "rating")
                      <*> (pure $ parseUserRating =<<   el ?<.> "user-rating")
                      <*> (pure $                       parseTags el ++ parseUserTags el)

instance FromXML Work where
  fromXML = undefined -- TODO

instance FromXML Label where
  fromXML = undefined -- TODO
        
instance FromXML Relation where
  fromXML = undefined --- TODO

instance FromXML RelationList where
  fromXML el = el <//=> ["relation"]

instance FromXML Recording where
  fromXML el = Recording <$> el !<@> "id"
                         <*> el !<|> "title"
                         <*> (pure $ (read . T.unpack) <$>   el ?<|> "length")
                         <*> (pure $                         el ?<|> "disambiguation")
                         <*> (el <//=> ["artist-credit", "name-credit"])
                         <*> (el <//=> ["release-list", "release"])
                         <*> (                         liftM2 (++) puids isrcs)
                         <*> (pure $ map parseRelationList $ el <//.> ["r el ation-list"])
                         <*> (pure $                         parseUserTags el ++ parseUserTags el)
                         <*> (pure $ parseRating =<<         el ?<.> "rating")
                         <*> (pure $ parseUserRating =<<     el ?<.> "user-rating")
    where puids        = mapM (idPath PUID) $ el <//.> ["puid-list", "puid"]
          isrcs        = mapM (idPath ISRC) $ el <//.> ["isrc-list", "isrc"]
          idPath con c = con <$> (c !<@> "id")

instance FromXML NameCredit where
  fromXML el = NameCredit <$> (pure $ el ?<@> "joinphrase")
                          <*> el !<|> "name"

instance FromXML Release where
  fromXML el = Release <$> el !<@> "id"
                       <*> el !<|> "title"
                       <*> (pure $ el ?<|> "status")
                       <*> (pure $ fromXML =<< el ?<.> "quality")
                       <*> (pure $ el ?<|> "disambiguation")
                       <*> (pure $ el ?<|> "packaging")
                       <*> (pure $ parseTextRepresentation <$> el ?<.> "text-representation")
                       <*> el <//=> ["artist-credit", "name-credit"]
                       <*> (pure $ parseDate =<<  el ?<|> "date")
                       <*> (pure $ (read . T.unpack) <$> el ?<|> "country")
                       <*> (pure $ el ?<|> "barcode")
                       <*> (pure $ ASIN <$> el ?<|> "asin")
                       <*> (pure $ mapM parseRelationList $ el <//.> ["relation-list"])

---- Helpers

-- Get attribute of cursor
(!<@>) :: F.Failure XmlException m => Cu.Cursor -> T.Text -> m T.Text
el !<@> n = forceEx ("missing " ++ T.unpack n) $ el $| laxAttribute n

(?<@>) :: Cu.Cursor -> T.Text -> Maybe T.Text
el ?<@> n =  listToMaybe $ el $| laxAttribute n

-- Shorthand to get content of named tag
(!<|>) :: F.Failure XmlException m => Cu.Cursor -> T.Text -> m T.Text
el !<|> n = forceEx ("missing " ++ T.unpack n) $ el $/ (cont <=< laxElement n)

-- Get content from the current element (custom unary not allowed)
cont :: F.Failure XmlException m => Cu.Cursor -> m T.Text
cont el = forceEx "no content" $ el $/ content

-- (Maybe) get content
(?<|>) :: Cu.Cursor -> T.Text -> Maybe T.Text
el ?<|> n = listToMaybe $ el $/ (laxElement n &/ content)
              -- Axis

-- (Maybe) find a node
(?<.>) :: Cu.Cursor -> T.Text -> Maybe Cursor
el ?<.> n = listToMaybe $ el $/ laxElement n

--REFACTOR: ! is inaccurate. It returns an array so there is no opportunity for failure
-- Find leaf nodes given list of paths to walk down
(<//.>) :: Cu.Cursor -> [T.Text] -> [Cu.Cursor]
el <//.> path = el $/ (foldl1 (&/) $ map laxElement path)

(<//=>) :: (FromXML a, Functor m, Applicative m, F.Failure XmlException m) => Cu.Cursor -> [T.Text] -> m [a]
el <//=> path = mapM fromXML $ el <//.> path

(!<//|>) :: Cu.Cursor -> [T.Text] -> [T.Text]
el !<//|> path = concatMap ($/ content) $ el <//.> path

forceEx :: F.Failure XmlException m => String -> [a] -> m a
forceEx = Cu.force . XmlException

newtype XmlException = XmlException { xmlErrorMessage :: String }
    deriving (Show, Typeable)

instance C.Exception XmlException

--TODO: refactor
parseLifeSpan :: F.Failure XmlException m => Maybe Cu.Cursor -> m LifeSpan
parseLifeSpan Nothing   = return (Nothing, Nothing)
parseLifeSpan (Just el) = return (pDate <$> fromEl "begin", pDate <$> fromEl "end")
  where pDate txt = (finalize . pad 3) $ splitOn "-" txt
        finalize (Just a:b:c:_) = PartialDate (read a) (read <$> b) (read <$> c)
        fromEl n = T.unpack <$> el ?<|> n

parseDate = undefined --TODO
parseRelationList = undefined --TODO

parseRating :: F.Failure XmlException m => Cu.Cursor -> m Rating
parseRating el = (return . uncurry) Rating `ap` parseRating' el

parseUserRating :: F.Failure XmlException m => Cu.Cursor -> m UserRating
parseUserRating el = (return . uncurry) UserRating `ap` parseRating' el

--TODO: refactor
parseRating' :: F.Failure XmlException m => Cu.Cursor -> m (Int, Float)
parseRating' el = do count <- el !<@> "votes-count"
                     let val = read . T.unpack . head $ el $/ content
                     return ((read . T.unpack) count, val)

parseUserTags :: Cu.Cursor -> [Tag]
parseUserTags el = UserTag <$> el !<//|> ["user-tag-list", "user-tag", "name"]

parseTags :: Cu.Cursor -> [Tag]
parseTags el = Tag <$> el !<//|> ["tag-list", "tag", "name"]

instance FromXML Quality where
  fromXML el = readQuality . T.unpack =<< el !<|> "value"
    where readQuality "high"   = return High
          readQuality "normal" = return Normal
          readQuality "low"    = return Low
          readQuality q        = fail $ "Unexpected quality " ++ q

parseTextRepresentation :: Cu.Cursor -> TextRepresentation
parseTextRepresentation el = TextRepresentation language script
  where language = el ?<|> "language"
        script   = el ?<|> "script"

pad :: Int -> [a] -> [Maybe a]
pad n xs = (Just <$> xs') ++ replicate (n - length xs) Nothing
  where xs' = take n xs

toI :: (Monad m) => m T.Text -> m Int
toI = liftM (read . T.unpack)

toF :: (Monad m) => m T.Text -> m Float
toF = liftM (read . T.unpack)
