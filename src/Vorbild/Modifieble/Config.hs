{-# LANGUAGE DeriveGeneric #-}

module Vorbild.Modifieble.Config
  ( ModifiebleFile(..)
  , BlockDescriptorItem(..)
  , ModifiebleParsingError(..)
  , BlockEdges(..)
  , readAndParseModifiebleConfigsFromJson
  ) where

import qualified Data.Text      as T

import           Data.Aeson     (FromJSON, ToJSON, eitherDecodeFileStrict)
import           GHC.Generics
import           Vorbild.Either (rightAccumulateWithList)

data ModifiebleFile =
  ModifiebleFile
    { filePath         :: FilePath
    , blockDescriptors :: [BlockDescriptorItem]
    }
  deriving (Generic, Show)

instance FromJSON ModifiebleFile

instance ToJSON ModifiebleFile

data BlockDescriptorItem =
  BlockDescriptorItem
    { blockLabel   :: Maybe String
    , edges   :: Maybe BlockEdges
    , actions :: [T.Text]
    }
  deriving (Generic, Show)

instance FromJSON BlockDescriptorItem

instance ToJSON BlockDescriptorItem

data BlockEdges =
  BlockEdges
    { start :: T.Text
    , end   :: T.Text
    }
  deriving (Generic, Show)

instance FromJSON BlockEdges

instance ToJSON BlockEdges

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
    mapper = foldr rightAccumulateWithList (Right [])
