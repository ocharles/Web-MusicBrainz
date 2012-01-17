module Main (main) where

import Test.Hspec (hspecX, descriptions)
import Control.Monad (msum)

import qualified Web.MusicBrainz.Testing.XML as X

main :: IO ()
main = hspecX $ descriptions [X.specs]
