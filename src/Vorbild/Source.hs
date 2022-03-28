module Vorbild.Source
  ( SourceAndContent(..)
  , Source(..)
  , InTmpValueParsingError(..)
  , generateFromTemplates
  , getSourcesRecursive
  , toSourceAndContent
  ) where

import           Control.Monad.Reader            (runReader)
import qualified Data.Map.Strict                 as Map
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Vorbild.Either
import           Vorbild.File
import           Vorbild.TemplateValue.Config
import           Vorbild.TemplateValue.Placement
import           Vorbild.TemplateValue.Segment

data SourceAndContent
  = Dir T.Text
  | FileAndContent T.Text T.Text
  deriving (Eq, Show)

data Source
  = SourceDir FilePath
  | SourceFile FilePath

data InTmpValueParsingError =
  InTmpValueParsingError
    { valueName :: String
    , path      :: String
    }
  deriving (Eq, Show)

generateFromTemplates ::
     PlaceholderConfig
  -> Map.Map TemplateValueId [TemplateValueSegment]
  -> [SourceAndContent]
  -> Either InTmpValueParsingError [SourceAndContent]
generateFromTemplates _ _ [] = Right $ []
generateFromTemplates config values sources =
  traverse (\s -> runReader (mapper s) (ValuesAndConfig values config)) sources
  where
    mapper source =
      case source of
        (Dir path) -> tryPlaceDir path
        (FileAndContent path content) ->
          tryPlaceFileAndContent path content

tryPlaceDir path = do
  result <- placeTemplateValues path
  let mapped =
        case result of
          Left e -> Left $ InTmpValueParsingError (T.unpack e) (T.unpack path)
          Right newTxt -> Right $ Dir newTxt
  pure $ mapped

tryPlaceFileAndContent path content = do
  pathResult <- placeTemplateValues path
  contentResult <- placeTemplateValues content
  pure $
    case (eitherZipWith FileAndContent pathResult contentResult) of
      Left e  -> Left $ InTmpValueParsingError (T.unpack e) (T.unpack path)
      Right s -> Right s

getSourcesRecursive :: FilePath -> IO [Source]
getSourcesRecursive dir = do
  (files, subdirs) <- splitOnFilesAndDirs dir
  if (subdirs == [] && files == [])
    then pure [SourceDir dir]
    else if (subdirs == [])
           then pure $ map (\file -> SourceFile file) files
           else fmap
                  (\other -> other <> map (\file -> SourceFile file) files)
                  (foldMap getSourcesRecursive subdirs)

toSourceAndContent :: [Source] -> IO [SourceAndContent]
toSourceAndContent sources = traverse mapper sources
  where
    mapper source =
      case source of
        (SourceDir path) -> pure $ Dir $ T.pack path
        (SourceFile path) ->
          fmap
            (\content -> FileAndContent (T.pack path) content)
            (TIO.readFile path)
