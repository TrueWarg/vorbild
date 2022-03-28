{-# LANGUAGE DeriveGeneric #-}

module Vorbild.Modifieble.Config
  ( ModifiebleFile(..)
  , BlockDescriptorItem(..)
  , ModifiebleParsingError(..)
  , readAndParseModifiebleConfigsFromJson
  ) where

import qualified Data.Text      as T

import           Data.Aeson     (FromJSON, ToJSON, eitherDecodeFileStrict)
import           GHC.Generics
import           Vorbild.Either (accumulateWithList)

data ModifiebleFile =
  ModifiebleFile
    { filePath         :: FilePath
    , rootDescriptor :: BlockDescriptorItem
    }
  deriving (Generic, Show)

instance FromJSON ModifiebleFile

instance ToJSON ModifiebleFile

data BlockDescriptorItem =
  BlockDescriptorItem
    { id         :: T.Text
    , start    :: T.Text
    , end      :: T.Text
    , actions  :: [T.Text]
    , children :: [BlockDescriptorItem]
    }
  deriving (Generic, Show)

instance FromJSON BlockDescriptorItem

instance ToJSON BlockDescriptorItem

data ModifiebleParsingError =
  ModifiebleParsingError
    { mfCause   :: String
    , mfSrcPath :: String
    }
  deriving (Show)

readAndParseModifiebleConfigsFromJson ::
     [FilePath] -> IO (Either ModifiebleParsingError [ModifiebleFile])
readAndParseModifiebleConfigsFromJson = (fmap mapper) . parse
  where
    parse =
      traverse
        (\path -> do
           result <-
             eitherDecodeFileStrict path :: IO (Either String ModifiebleFile)
           case result of
             Left e       -> pure $ Left $ ModifiebleParsingError e path
             Right result -> pure $ Right result)
    mapper = foldl accumulateWithList (Right [])

