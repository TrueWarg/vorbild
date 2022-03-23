{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Modifieble.Config where

import qualified Data.Text    as T

import           Data.Aeson   (FromJSON, ToJSON, eitherDecodeFileStrict)
import           GHC.Generics

data ModifiebleFile =
  ModifiebleFile
    { path           :: FilePath
    , rootDescriptor :: BlockDescriptorItem
    }
  deriving (Generic, Show)

instance FromJSON ModifiebleFile

instance ToJSON ModifiebleFile

data BlockDescriptorItem =
  BlockDescriptorItem
    { id        :: T.Text
    , dStart    :: Maybe T.Text
    , dEnd      :: Maybe T.Text
    , dActions  :: [T.Text]
    , dChildren :: [BlockDescriptorItem]
    }
  deriving (Generic, Show)

instance FromJSON BlockDescriptorItem

instance ToJSON BlockDescriptorItem

data ModifiebleParsingError =
  ModifiebleParsingError
    { cause   :: String
    , srcPath :: String
    }
  deriving (Show)

readAndParseConfigsFromJson ::
     [FilePath] -> IO [Either ModifiebleParsingError ModifiebleFile]
readAndParseConfigsFromJson paths =
  traverse
    (\path -> do
       result <-
         eitherDecodeFileStrict path :: IO (Either String ModifiebleFile)
       case result of
         Left e       -> pure $ Left $ ModifiebleParsingError e path
         Right result -> pure $ Right result)
    paths
