{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language MultiParamTypeClasses #-}
{-# language InstanceSigs #-}

module Lib
    ( getPhoto
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

instance ToJSON Photo 
instance FromJSON Photo
  
getPhoto :: IO [Photo]
getPhoto = do
  response <- httpJSON "https://jsonplaceholder.typicode.com/photos" :: IO (Response [Photo])
  return $ getResponseBody response


