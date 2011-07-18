{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable #-}
module Audio.MusicBrainz.XML (deepPath,
                              parseArtist,
                              parseRating, --DEBUG
                              maybeNode, --DEBUG
                              forceAttribute, --DEBUG
                              ) where

import           Data.List.Split (splitOn)
import           Data.Maybe (listToMaybe)
import           Data.Typeable
import           Data.XML.Types
import qualified Control.Failure            as F
import           Text.XML.Enumerator.Resolved (Name(..))
import qualified Data.Text as T
import           Text.XML.Enumerator.Cursor (($/), (&|), (>=>))
import           Text.XML.Enumerator.Cursor
import qualified Text.XML.Enumerator.Cursor as Cu
import           Text.XML.Enumerator.Resolved
import qualified Control.Exception            as C

import Control.Applicative
import Control.Monad

import Audio.MusicBrainz.Types

parseArtist :: F.Failure XmlException m => Cu.Cursor -> m Artist
parseArtist el = do n      <- forceContent "name" el
                    i      <- forceAttribute "id" el
                    t      <- forceAttribute "type" el
                    ls     <- parseLifeSpan $ maybeNode "life-span" el
                    let r  = parseRating =<< maybeNode "rating" el
                    let sn = maybeContent "sort-name" el
                    let g  = maybeContent "gender" el
                    let d  = maybeContent "disambiguation" el
                    let c  = (read . T.unpack) <$> maybeContent "country" el
                    let as = concatMap ($/ content) $ deepPath el ["alias-list", "alias"]
                    let ts = map Tag $ concatMap ($/ content) $ deepPath el ["tag-list", "tag", "name"]
                    return Artist { artistId             = i,
                                    artistName           = n,
                                    artistType           = t,
                                    artistAliases        = as,
                                    artistSortName       = sn,
                                    artistLifeSpan       = ls,
                                    artistCountry        = c,
                                    artistGender         = g,
                                    artistDisambiguation = d,
                                    --TODO
                                    artistRecordings   = [],
                                    artistReleases     = [],
                                    artistLabels       = [],
                                    artistWorks        = [],
                                    artistRelationLists= [],
                                    artistUserTags     = [],
                                    artistRating       = r,
                                    artistUserRating   = Nothing,

                                    artistTags           = ts }

---- Helpers

elContent :: T.Text -> Cursor -> [T.Text]
elContent name = laxElement name &/ content

forceEx :: F.Failure XmlException m => String -> [a] -> m a
forceEx = Cu.force . XmlException

newtype XmlException = XmlException { xmlErrorMessage :: String }
    deriving (Show, Typeable)

instance C.Exception XmlException

deepPath :: Cu.Cursor -> [T.Text] -> [Cu.Cursor]
deepPath el path = ($/) el $ foldl1 (&/) $ map laxElement path

--TODO: on lists, use maybe to default to empty list or something
forceContent :: F.Failure XmlException m => T.Text -> Cu.Cursor -> m T.Text
forceContent n el = forceEx ("missing " ++ T.unpack n) $ el $/ elContent n

forceAttribute :: F.Failure XmlException m => T.Text -> Cu.Cursor -> m T.Text
forceAttribute n el = forceEx ("missing attribute " ++ T.unpack n) $ el $| laxAttribute n 

forceNode :: F.Failure XmlException m => T.Text -> Cu.Cursor -> m Cu.Cursor
forceNode n el = forceEx ("missing " ++ T.unpack n) $ el $/ laxElement n

maybeContent :: T.Text -> Cu.Cursor -> Maybe T.Text
maybeContent n el = listToMaybe $ el $/ elContent n

maybeNode :: T.Text -> Cu.Cursor -> Maybe Cu.Cursor
maybeNode n el = listToMaybe $ el $/ laxElement n

parseLifeSpan :: F.Failure XmlException m => Maybe Cu.Cursor -> m LifeSpan
parseLifeSpan Nothing   = return (Nothing, Nothing)
parseLifeSpan (Just el) = return (pDate <$> fromEl "begin", pDate <$> fromEl "end")
  where pDate txt = (finalize . pad 3) $ splitOn "-" txt
        finalize (Just a:b:c:_) = PartialDate (read a) (read <$> b) (read <$> c)
        fromEl n = T.unpack <$> (listToMaybe $ el $/ laxElement n &/ content)

parseRating :: F.Failure XmlException m => Cu.Cursor -> m Rating
parseRating el = (return . uncurry) Rating `ap` parseRating' el

parseUserRating :: F.Failure XmlException m => Cu.Cursor -> m UserRating
parseUserRating el = (return . uncurry) UserRating `ap` parseRating' el

parseRating' :: F.Failure XmlException m => Cu.Cursor -> m (Int, Float)
parseRating' el = do count <- forceAttribute "votes-count" el
                     let val = read . T.unpack . head $ el $/ content
                     return ((read . T.unpack) count, val)


pad :: Int -> [a] -> [Maybe a]
pad n xs = (Just <$> xs') ++ replicate (n - length xs) Nothing
  where xs' = take n xs
