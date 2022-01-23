{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Field
 where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.HashMap as HashMap

data Field 
    = Field
    {
        name :: Text.Text
      , label :: Maybe Text.Text
    }
    deriving Show

readAndParseFields :: FilePath -> IO [Field]
readAndParseFields path = do
    content <- fmap Text.lines $ Text.readFile path
    let
        transform = \line ->
            let
                (field, label) = Text.break (\ch -> ch == ':') line
                processedField = Text.strip field
                processedLabel = Text.strip $ Text.drop 1 label
                mLabel = if (processedLabel == Text.empty) 
                    then Nothing 
                    else Just processedLabel
            in
                Field processedField mLabel
        fields = map transform content
    pure fields  
    
fillFields :: [Field] -> IO (HashMap.Map Text.Text Text.Text)
fillFields fields = do
    let 
        transform = \field -> do
            let 
                hint = case (label field) of
                    Nothing -> "Specify: " <> name field
                    Just txt -> txt
            Text.putStrLn hint
            value <- Text.getLine
            pure $ ((name field), value)

    filledFields <- traverse transform fields
    pure $ HashMap.fromList filledFields  
