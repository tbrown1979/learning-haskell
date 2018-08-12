{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language MultiParamTypeClasses #-}
{-# language InstanceSigs #-}

module Mp3
    (
      listRequests,
      Entries(..),
      createMp3DownloaderAlg,
      Mp3DownloaderAlg(..)
    ) where

import Data.Aeson                 (FromJSON, ToJSON)
import Network.HTTP.Simple
import GHC.Generics
import qualified Data.Text as T
import Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Rank2
import System.IO
import Data.ByteString.Char8      (pack, append)

dropboxAuthToken :: IO DropboxAuthToken
dropboxAuthToken = do
  _ <- putStrLn "getting dropboxAuthToken"
  file <- readFile "dropboxAuthToken.txt"
  return DropboxAuthToken { token = T.strip $ T.pack file }

dropboxRequest :: DropboxAuthToken -> Request
dropboxRequest t = setRequestMethod "POST"
  $ setRequestBodyLBS "{\"path\": \"/lofi-radio\",\"recursive\": false,\"include_media_info\": false,\"include_deleted\": false,\"include_has_explicit_shared_members\": false,\"include_mounted_folders\": true}"
  $ setRequestHeader "Content-Type" ["application/json"]
  $ setRequestHeader "Authorization" [append "Bearer " $ pack $ T.unpack $ token t]
  $ setRequestSecure True
  $ "https://api.dropboxapi.com/2/files/list_folder"

data Mp3 = Mp3 {
  name :: T.Text,
  id :: T.Text
} deriving (Show, Generic)
instance ToJSON Mp3
instance FromJSON Mp3

data Entries = Entries {
  entries :: [Mp3]
} deriving (Show, Generic)
instance ToJSON Entries
instance FromJSON Entries

listRequests :: DropboxAuthToken -> IO Entries
listRequests token = do
  response <- httpJSON $ dropboxRequest token :: IO (Response Entries)
  return $ getResponseBody response

newtype DropboxAuthToken = DropboxAuthToken {
  token :: T.Text
} deriving Generic

data Mp3DownloaderAlg f where
  Mp3DownloaderAlg :: {
    listRequests_ :: f Entries
  } -> Mp3DownloaderAlg f

createMp3DownloaderAlg :: MonadIO f => f (Mp3DownloaderAlg f)
createMp3DownloaderAlg = do
  t <- liftIO dropboxAuthToken
  return Mp3DownloaderAlg {
    listRequests_ = liftIO $ listRequests t
    }
