{-# LANGUAGE DeriveGeneric #-}

module Vorbild.TemplateValue.Config where

import           Data.Aeson      (FromJSON, ToJSON, eitherDecodeFileStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           GHC.Generics

data ValueConfigItem =
  ValueConfigItem
    { name  :: ValueName
    , label :: Maybe T.Text
    , value :: Maybe RawValue
    }
  deriving (Generic, Show)

data ConfigParsingError =
  ConfigParsingError
    { cause   :: String
    , srcPath :: String
    }
  deriving Show

instance FromJSON ValueConfigItem

instance ToJSON ValueConfigItem

type ValueName = T.Text

type RawValue = T.Text

data PlaceholderConfig =
  PlaceholderConfig
    { openTag           :: T.Text
    , closeTag          :: T.Text
    , valuePrefix       :: T.Text
    , modifierSeparator :: T.Text
    }
  deriving (Generic, Show)

instance FromJSON PlaceholderConfig

instance ToJSON PlaceholderConfig

readAndParseConfigItemsFromJson ::
     FilePath -> IO (Either ConfigParsingError [ValueConfigItem])
readAndParseConfigItemsFromJson path = do
  fields <- eitherDecodeFileStrict path :: IO (Either String [ValueConfigItem])
  case fields of
    Left e       -> pure $ Left $ ConfigParsingError e path
    Right result -> pure $ Right result

prepareRawValues :: [ValueConfigItem] -> IO (Map.Map ValueName RawValue)
prepareRawValues fields = do
  let transform field =
        case (value field) of
          (Just v) -> pure (name field, v)
          Nothing -> do
            let hint =
                  case (label field) of
                    Nothing  -> T.pack "Specify: " <> name field
                    Just txt -> txt
            T.putStrLn hint
            input <- T.getLine
            pure (name field, input)
  filledFields <- traverse transform fields
  pure $ Map.fromList filledFields

readAndParsePlaceholderConfigFromJson ::
     FilePath -> IO (Either ConfigParsingError PlaceholderConfig)
readAndParsePlaceholderConfigFromJson path = do
  config <- eitherDecodeFileStrict path :: IO (Either String PlaceholderConfig)
  case config of
    Left e       -> pure $ Left $ ConfigParsingError e path
    Right result -> pure $ Right result
