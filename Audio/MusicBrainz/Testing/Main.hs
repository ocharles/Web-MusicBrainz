module Main (main) where

import Test.Hspec (hspecX, descriptions)
import Control.Monad (msum)

import qualified Audio.MusicBrainz.Testing.XML as X

main :: IO ()
main = hspecX $ descriptions [X.specs]
