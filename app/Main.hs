module Main where

import Lib                        (PhotoAlg(..), Photo, getAllPhotos, createPhotoAlg)
import Control.Monad.IO.Class     (MonadIO, liftIO)

blah :: MonadIO f => PhotoAlg f -> f Photo
blah pAlg = fmap head $ listN_ pAlg 5

main :: IO ()
-- main = getAllPhotos >>= (putStrLn . show . take 5)
main = blah createPhotoAlg >>= (putStrLn . show)
