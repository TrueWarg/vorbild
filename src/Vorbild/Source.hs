{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Source
  ( SourceAndContent(..)
  , Source(..)
  , InTmpValueParsingError(..)
  , generateFromTemplates
  , getSourcesRecursive
  , toSourceAndContent
  ) where

import           System.Directory               (listDirectory)

import           Control.Monad.Reader           (Reader, ask, runReader)
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           System.FilePath                ((</>))
import           Vorbild.Either
import           Vorbild.File
import           Vorbild.TemplateValue.Config
import           Vorbild.TemplateValue.Modifier (Modifier (..), applyModifiers,
                                                 tryParseModifier)
import           Vorbild.TemplateValue.Segment
import           Vorbild.Text                   (splitOnAnyOf)

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
  traverse (\s -> runReader (mapper s) config) sources
  where
    mapper source =
      case source of
        (Dir path) -> tryPlaceDir values path
        (FileAndContent path content) ->
          tryPlaceFileAndContent values path content

tryPlaceDir values path = do
  result <- placeTemplateValues values path
  let mapped =
        case result of
          Left e -> Left $ InTmpValueParsingError (T.unpack e) (T.unpack path)
          Right newTxt -> Right $ Dir newTxt
  pure $ mapped

tryPlaceFileAndContent values path content = do
  pathResult <- placeTemplateValues values path
  contentResult <- placeTemplateValues values content
  pure $
    case (eitherZipWith FileAndContent pathResult contentResult) of
      Left e  -> Left $ InTmpValueParsingError (T.unpack e) (T.unpack path)
      Right s -> Right s

-- There is idea to represent all text as TemplateSegment to easy modify text
-- and for better generalisation
-- But need to determinate start of recursive calculation.
-- Currently this start func placeTemplateValues.
-- todo: research this idea (it's realy needed? How to impliment it better? etc.)
placeTemplateValues ::
     Map.Map TemplateValueId [TemplateValueSegment]
  -> T.Text
  -> Reader PlaceholderConfig (Either T.Text T.Text)
placeTemplateValues values txt = do
  config <- ask
  let oTeg = openTag config
      cTeg = closeTag config
      prefix = valuePrefix config
      separator = modifierSeparator config
      statement chunk = T.drop (T.length prefix) chunk
      chunks =
        filter (\chunk -> not $ T.null chunk) (splitOnAnyOf [oTeg, cTeg] txt)
      parseSegments chunk =
        let (valueName, modifiersBlock) = T.breakOn separator (statement chunk)
            inSourceModifiers = parseModifiers separator modifiersBlock
         in case (Map.lookup (TemplateValueId valueName) values) of
              Just segments ->
                Right $
                applyModifiers (readValueSegmentList segments) inSourceModifiers
              Nothing -> Left valueName
      transform acc chunk =
        if (T.isPrefixOf prefix chunk)
          then accumulate acc (parseSegments chunk)
          else accumulate acc (Right chunk)
      results = foldl transform (Right "") chunks
  pure $ results

-- todo: add error for parseModifiers?
parseModifiers :: T.Text -> T.Text -> [Modifier]
parseModifiers separator modifiersBlock =
  let dropFirstSeparator block = T.drop (T.length separator) block
      modifiersCodes = T.splitOn separator (dropFirstSeparator modifiersBlock)
      maybeModifiers = map tryParseModifier modifiersCodes
      onlyParsed = filter (\item -> item /= Nothing) maybeModifiers
   in map (\(Just modifier) -> modifier) onlyParsed

getSourcesRecursive :: FilePath -> IO [Source]
getSourcesRecursive dir = do
  content <- listDirectory dir
  (files, subdirs) <- splitOnFilesAndDirs $ fmap (\item -> dir </> item) content
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
