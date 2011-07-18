{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable #-}
module Audio.MusicBrainz.XML where--DEBUG
--module Audio.MusicBrainz.XML (parseArtist, (!<//.>)) where

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

import Audio.MusicBrainz.Types

parseArtist :: (Functor m, Applicative m,  F.Failure XmlException m) => Cu.Cursor -> m Artist
parseArtist el = Artist <$> (el !<@> "id")
                        <*> (el !<@> "type")
                        <*> (el !<|> "name")
                        <*> (mapM cont $                  el !<//.> ["alias-list", "alias"])
                        <*> (pure $                       el ?<|> "sort-name")
                        <*> (parseLifeSpan $              el ?<.> "life-span")
                        <*> (pure $ (read . T.unpack) <$> el ?<|> "country")
                        <*> (pure $                       el ?<|> "gender")
                        <*> (pure $                       el ?<|> "disambiguation")
                        --TODO
                        <*> (pure $                       []) -- Recordings
                        <*> (pure $                       []) -- Releases
                        <*> (pure $                       []) -- Labels
                        <*> (pure $                       []) -- Works
                        <*> (pure $                       []) -- RelationLists
                        <*> (pure $                       []) -- UserTags

                        <*> (pure $ parseRating =<<       el ?<.> "rating")
                        <*> (pure $ parseUserRating =<<       el ?<.> "user-rating")
                        <*> (pure $ Tag <$>               el !<//|> ["tag-list", "tag", "name"])

---- Helpers

-- Get attribute of cursor
(!<@>) :: F.Failure XmlException m => Cu.Cursor -> T.Text -> m T.Text
el !<@> n = forceEx ("missing " ++ T.unpack n) $ el $| laxAttribute n

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

-- Find leaf nodes given list of paths to walk down
(!<//.>) :: Cu.Cursor -> [T.Text] -> [Cu.Cursor]
el !<//.> path = el $/ (foldl1 (&/) $ map laxElement path)

(!<//|>) :: Cu.Cursor -> [T.Text] -> [T.Text]
el !<//|> path = concatMap ($/ content) $ el !<//.> path

elContent :: T.Text -> Cursor -> [T.Text]
elContent name = laxElement name &/ content

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

parseRating :: F.Failure XmlException m => Cu.Cursor -> m Rating
parseRating el = (return . uncurry) Rating `ap` parseRating' el

parseUserRating :: F.Failure XmlException m => Cu.Cursor -> m UserRating
parseUserRating el = (return . uncurry) UserRating `ap` parseRating' el

--TODO: refactor
parseRating' :: F.Failure XmlException m => Cu.Cursor -> m (Int, Float)
parseRating' el = do count <- el !<@> "votes-count"
                     let val = read . T.unpack . head $ el $/ content
                     return ((read . T.unpack) count, val)

pad :: Int -> [a] -> [Maybe a]
pad n xs = (Just <$> xs') ++ replicate (n - length xs) Nothing
  where xs' = take n xs

toI :: (Monad m) => m T.Text -> m Int
toI = liftM (read . T.unpack)

toF :: (Monad m) => m T.Text -> m Float
toF = liftM (read . T.unpack)
