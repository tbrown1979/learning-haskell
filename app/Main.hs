module Main where

import Lib                        (PhotoAlg(..), Photo, getAllPhotos, createPhotoAlg)
import Mp3                        (createMp3DownloaderAlg, Entries(..), Mp3DownloaderAlg(..))
import Control.Monad.IO.Class     (MonadIO, liftIO)
import System.IO  


blah :: MonadIO f => PhotoAlg f -> f Photo
blah pAlg = fmap head $ listN_ pAlg 5

main :: IO ()
-- main = listRequests >>= putStrLn . show . length . entries
-- main = do
--   handle <- openFile "dropboxAuthToken.txt" ReadMode  
--   contents <- hGetContents handle  
--   putStr contents  
--   hClose handle
main = do
  mp3Alg <- createMp3DownloaderAlg
  requests <- listRequests_ mp3Alg
  putStrLn $ show $ take 5 $ entries requests
  
