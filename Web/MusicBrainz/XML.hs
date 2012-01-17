{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Web.MusicBrainz.XML
    ( ) where

import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.Failure (Failure)
import Control.Monad (ap, liftM, liftM2)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML.Cursor (Cursor, ($/), content)

import Text.XML.Cursor.FromXML
import Web.MusicBrainz.Types

instance FromXML Artist where
  fromXML el =
    Artist <$> el !<@> "id"
           <*> (pure $ el ?<@> "type")
           <*> el !<|> "name"
           <*> (pure $ el !<//|> ["alias-list", "alias"])
           <*> (pure $ el ?<|> "sort-name")
           <*> (parseLifeSpan $ el ?<.> "life-span")
           <*> (pure $ (read . T.unpack) <$> el ?<|> "country")
           <*> (pure $ el ?<|> "gender")
           <*> (pure $ el ?<|> "disambiguation")
           <*> el <//=> ["recording-list", "recording"]
           <*> el <//=> ["release-list", "release"]
           <*> el <//=> ["label-list", "label"]
           <*> el <//=> ["work-list", "work"]
           <*> el <//=> ["relation-list"]
           <*> (pure $ listToMaybe =<< el <//=> ["rating"])
           <*> (pure $ listToMaybe =<< el <//=> ["user-rating"])
           <*> (pure $ parseTags el ++ parseUserTags el)

instance FromXML Work where
  fromXML el =
    Work <$> el !<@> "id"
         <*> (pure $ el ?<@> "type")
         <*> el !<|> "title"
         <*> el <//=> ["artist-credit", "name-credit"]
         <*> (pure $ ISWC <$>        el ?<|> "iswc")
         <*> (pure $ el ?<|> "disambiguation")
         <*> (pure $ el !<//|> ["alias-list", "alias"])
         <*> el <//=> ["relation-list"]
         <*> (pure $ parseTags el ++ parseUserTags el)
         <*> (pure $ listToMaybe =<< el <//=> ["rating"])
         <*> (pure $ listToMaybe =<< el <//=> ["user-rating"])

instance FromXML Label where
  fromXML el =
    Label <$> el !<@> "id"
          <*> (pure $ el ?<@> "type")
          <*> el !<|> "name"
          <*> (pure $ el ?<|> "sort-name")
          <*> (pure $ (read . T.unpack) <$> el ?<|> "label-code")
          <*> (pure $ el ?<|> "disambiguation")
          <*> (pure $ (read . T.unpack) <$> el ?<|> "country")
          <*> (pure $ parseLifeSpan $ el ?<.> "life-span")
          <*> (pure $ el !<//|> ["alias-list", "alias"])
          <*> el <//=> ["relation-list"]
          <*> (pure $ parseTags el ++ parseUserTags el)
          <*> (pure $ listToMaybe =<< el <//=> ["rating"])
          <*> (pure $ listToMaybe =<< el <//=> ["user-rating"])

instance FromXML MBID where
  fromXML el = MBID <$> cont el

instance FromXML RelationList where
  fromXML = parseRelationList

instance FromXML LifeSpan where
  fromXML el = return (pDate <$> fromEl "begin", pDate <$> fromEl "end")
    where pDate txt               = (finalize . pad 3) $ splitOn "-" txt
          finalize (Just a:b:c:_) = PartialDate (read a) (read <$> b) (read <$> c)
          fromEl n                = T.unpack <$> el ?<|> n

instance FromXML Recording where
  fromXML el = Recording <$>                                 el !<@> "id"
                         <*>                                 el !<|> "title"
                         <*> (pure $ (read . T.unpack) <$>   el ?<|> "length")
                         <*> (pure $                         el ?<|> "disambiguation")
                         <*>                                 el <//=> ["artist-credit", "name-credit"]
                         <*>                                 el <//=> ["release-list", "release"]
                         <*> (                               liftM2 (++) puids isrcs)
                         <*> (mapM parseRelationList $       el <//.> ["relation-list"])
                         <*> (pure $                         parseUserTags el ++ parseUserTags el)
                         <*> (pure $ listToMaybe =<<         el <//=> ["rating"])
                         <*> (pure $ listToMaybe =<<         el <//=> ["user-rating"])
    where puids        = mapM (idPath PUID) $ el <//.> ["puid-list", "puid"]
          isrcs        = mapM (idPath ISRC) $ el <//.> ["isrc-list", "isrc"]
          idPath con c = con <$> (c !<@> "id")

instance FromXML NameCredit where
  fromXML el = NameCredit <$> (pure $ el ?<@> "joinphrase")
                          <*>         el !<|> "name"

instance FromXML Release where
  fromXML el = Release <$>                               el !<@> "id"
                       <*>                               el !<|> "title"
                       <*> (pure $                       el ?<|> "status")
                       <*> (pure $                       el ?<=> "quality")
                       <*> (pure $                       el ?<|> "disambiguation")
                       <*> (pure $                       el ?<|> "packaging")
                       <*> (pure $ listToMaybe =<<       el <//=> ["text-representation"])
                       <*>                               el <//=> ["artist-credit", "name-credit"]
                       <*> (pure $ listToMaybe =<<       el <//=> ["release-group"])
                       <*> (pure $ parseDate =<<         el ?<|> "date")
                       <*> (pure $ (read . T.unpack) <$> el ?<|> "country")
                       <*> (pure $                       el ?<|> "barcode")
                       <*> (pure $ ASIN <$>              el ?<|> "asin")
                       <*> (mapM parseRelationList $     el <//.> ["relation-list"])

instance FromXML ReleaseGroup where
  fromXML el = ReleaseGroup <$>                           el !<@> "id"
                            <*> (pure $                   el ?<@> "type")
                            <*>                           el !<|> "title"
                            <*> (pure $                   el ?<|> "disambiguation")
                            <*> (pure $                   el ?<|> "comment")
                            <*> (pure $ parseDate =<<     el ?<|> "release-date")
                            <*>                           el <//=> ["artist-credit", "name-credit"]
                            <*>                           el <//=> ["release-list", "release"]
                            <*> (mapM parseRelationList $ el <//.> ["relation-list"])
                            <*> (pure $                   parseUserTags el ++ parseUserTags el)
                            <*> (pure $ listToMaybe =<<   el <//=> ["rating"])
                            <*> (pure $ listToMaybe =<<   el <//=> ["user-rating"])

instance FromXML Quality where
  fromXML el = readQuality . T.unpack =<< el !<|> "value"
    where readQuality "high"   = return High
          readQuality "normal" = return Normal
          readQuality "low"    = return Low
          readQuality q        = fail $ "Unexpected quality " ++ q

instance FromXML Direction where
  fromXML el = readDirection . T.unpack =<< el !<|> "value"
    where readDirection "both"     = return Both
          readDirection "forward"  = return Forward
          readDirection "backward" = return Backward
          readDirection d          = fail $ "Unexpected direction " ++ d

instance FromXML Rating where
  fromXML el = (return . uncurry) Rating `ap` parseRating' el

instance FromXML UserRating where
  fromXML el = (return . uncurry) UserRating `ap` parseRating' el

instance FromXML TextRepresentation where
  fromXML el = return $ TextRepresentation language script
    where language = el ?<|> "language"
          script   = el ?<|> "script"

---- Helpers
parseRelationList :: (Functor m, Applicative m, Failure XmlException m) => Cursor -> m RelationList
parseRelationList el = do tt <- el !<@> "target-type"
                          mapM (parseRelation $ T.unpack tt) rels
  where rels = el <//.> ["relation"]

parseRelation :: (Functor m, Applicative m, Failure XmlException m) => String -> Cursor -> m Relation
parseRelation "artist"        el = parseArtistRelation el
parseRelation "release"       el = parseReleaseRelation el
parseRelation "release-group" el = parseReleaseGroupRelation el
parseRelation "recording"     el = parseRecordingRelation el
parseRelation "label"         el = parseLabelRelation el
parseRelation "work"          el = parseWorkRelation el
parseRelation t               el = fail $ "Unexpected target-type " ++ t

parseRelation' :: (Functor m, Applicative m, Failure XmlException m, FromXML a)
                  => (Text -> MBID -> LifeSpan -> Maybe Direction -> a -> Relation)
                  -> Cursor
                  -> Text
                  -> m Relation
parseRelation' con el n = con <$> el !<|> "type"
                              <*> (el !<=> "target")
                              <*> fromXML el
                              <*> (pure $ el ?<=> "direction")
                              <*> el !<=> n


parseArtistRelation :: (Functor m, Applicative m,  Failure XmlException m) => Cursor -> m Relation
parseArtistRelation el = parseRelation' ArtistRelation el "artist"


parseReleaseRelation :: (Functor m, Applicative m,  Failure XmlException m) => Cursor -> m Relation
parseReleaseRelation el = parseRelation' ReleaseRelation el "release"

parseReleaseGroupRelation :: (Functor m, Applicative m,  Failure XmlException m) => Cursor -> m Relation
parseReleaseGroupRelation el = parseRelation' ReleaseGroupRelation el "release-group"

parseRecordingRelation :: (Functor m, Applicative m,  Failure XmlException m) => Cursor -> m Relation
parseRecordingRelation el = parseRelation' RecordingRelation el "recording"

parseLabelRelation :: (Functor m, Applicative m,  Failure XmlException m) => Cursor -> m Relation
parseLabelRelation el = parseRelation' LabelRelation el "label"

parseWorkRelation :: (Functor m, Applicative m,  Failure XmlException m) => Cursor -> m Relation
parseWorkRelation el = parseRelation' WorkRelation el "work"

--TODO: refactor
parseDate = undefined --TODO

parseLifeSpan :: (Functor m, Applicative m,  Failure XmlException m) => Maybe Cursor -> m LifeSpan
parseLifeSpan Nothing   = return (Nothing, Nothing)
parseLifeSpan (Just el) = fromXML el

--TODO: refactor
parseRating' :: Failure XmlException m => Cursor -> m (Int, Float)
parseRating' el = do count <- el !<@> "votes-count"
                     let val = read . T.unpack . head $ el $/ content
                     return ((read . T.unpack) count, val)

parseUserTags :: Cursor -> [Tag]
parseUserTags el = UserTag <$> el !<//|> ["user-tag-list", "user-tag", "name"]

parseTags :: Cursor -> [Tag]
parseTags el = Tag <$> el !<//|> ["tag-list", "tag", "name"]

pad :: Int -> [a] -> [Maybe a]
pad n xs = (Just <$> xs') ++ replicate (n - length xs) Nothing
  where xs' = take n xs

toI :: (Monad m) => m Text -> m Int
toI = liftM (read . T.unpack)

toF :: (Monad m) => m Text -> m Float
toF = liftM (read . T.unpack)
