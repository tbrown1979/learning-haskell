{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language MultiParamTypeClasses #-}
{-# language InstanceSigs #-}

module Lib
    ( getAllPhotos,
      PhotoAlg(..),
      Photo,
      createPhotoAlg
    ) where

import Data.Aeson                 (FromJSON, ToJSON)
import Network.HTTP.Simple
import GHC.Generics
import qualified Data.Text as T
import Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Rank2

data Photo = Photo {
  albumId :: Int,
  id :: Int,
  title :: T.Text,
  url :: T.Text,
  thumbnailUrl :: T.Text
} deriving (Show, Generic)

data PhotoAlg f where 
  PhotoAlg :: {
      getAllPhotos_ :: f [Photo]
    , listN_ :: Int -> f [Photo]
  } -> PhotoAlg f

createPhotoAlg :: MonadIO f => PhotoAlg f
createPhotoAlg = PhotoAlg {
  getAllPhotos_ = liftIO getAllPhotos,
  listN_ = \n -> liftIO (listN n)
}

instance ToJSON Photo 
instance FromJSON Photo

photoUrl :: Request
photoUrl = "https://jsonplaceholder.typicode.com/photos"
  
getAllPhotos :: IO [Photo]
getAllPhotos = do
  response <- httpJSON photoUrl :: IO (Response [Photo])
  return $ getResponseBody response

listN :: Int -> IO [Photo]
listN n = fmap (take n) getAllPhotos
