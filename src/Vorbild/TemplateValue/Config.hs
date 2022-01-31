{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Vorbild.TemplateValue.Config
 where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map.Strict as Map
import GHC.Generics
import Data.Aeson
import Control.Exception

data ValueConfigItem 
    = ValueConfigItem
    {
        name :: ValueName
      , label :: Maybe Text.Text
      , value :: Maybe RawValue
    }
    deriving (Generic, Show)

instance FromJSON ValueConfigItem
instance ToJSON ValueConfigItem

type ValueName = Text.Text
type RawValue = Text.Text

readAndParseConfigItemsFromJson :: FilePath -> IO [ValueConfigItem]
readAndParseConfigItemsFromJson path = do
    fields <- eitherDecodeFileStrict path :: IO (Either String [ValueConfigItem])
    case fields of
        Left e -> throwIO (userError e)
        Right result -> pure result
 

prepareRawValues :: [ValueConfigItem] -> IO (Map.Map ValueName RawValue)
prepareRawValues fields = do
    let 
        transform = \field -> 
            case (value field) of 
                (Just v) -> pure (name field, v)
                Nothing -> do
                    let
                        hint = case (label field) of
                            Nothing -> "Specify: " <> name field
                            Just txt -> txt
                    Text.putStrLn hint
                    input <- Text.getLine
                    pure (name field, input)

    filledFields <- traverse transform fields
    pure $ Map.fromList filledFields  
