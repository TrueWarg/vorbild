{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Source
  ( SourceAndContent(..)
  , Source(..)
  , generateFromTemplates
  , placeTemplateValues
  , getSourcesRecursive
  , toSourceAndContent
  ) where

import           System.Directory              (doesFileExist, listDirectory)

import           Control.Monad                 (filterM)
import           Data.List                     ((\\))
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.FilePath               ((</>))
import           Vorbild.Text                  (splitOnAnyOf)

import           Vorbild.TemplateValue.Config
import           Vorbild.TemplateValue.Segment

data SourceAndContent
  = Dir T.Text
  | FileAndContent T.Text T.Text

data Source
  = SourceDir FilePath
  | SourceFile FilePath

generateFromTemplates ::
     PlaceholderConfig
  -> Map.Map TemplateValueId [TemplateValueSegment]
  -> [SourceAndContent]
  -> [SourceAndContent]
generateFromTemplates _ _ [] = []
generateFromTemplates config values sources = map mapper sources
  where
    mapper source =
      case source of
        (Dir path) -> Dir $ placeTemplateValues config values path
        (FileAndContent path content) ->
          FileAndContent
            (placeTemplateValues config values path)
            (mconcat $
             map
               (\line -> placeTemplateValues config values line <> "\n")
               (T.lines content))

-- There is idea to represent all text as TemplateSegment to easy modify text
-- and for better generalisation
-- But need to determinate start of recursive calculation.
-- Currently this start func placeTemplateValues.
-- todo: research this idea (it's realy needed? How to impliment it better? etc.)
placeTemplateValues ::
     PlaceholderConfig
  -> Map.Map TemplateValueId [TemplateValueSegment]
  -> T.Text
  -> T.Text
placeTemplateValues config values txt =
  let oTeg = openTag config
      cTeg = closeTag config
      prefix = valuePrefix config
      prefixLength = T.length prefix
      chunks =
        filter (\chunk -> not $ T.null chunk) (splitOnAnyOf [oTeg, cTeg] txt)
      transform chunk acc =
        if (T.isPrefixOf prefix chunk)
          then (readValueSegmentList $
                values Map.! TemplateValueId (T.drop prefixLength chunk)) <>
               acc
          else chunk <> acc
   in foldr transform "" chunks

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

splitOnFilesAndDirs :: [FilePath] -> IO ([FilePath], [FilePath])
splitOnFilesAndDirs paths = do
  files <- filterM doesFileExist paths
  pure (files, paths \\ files)

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
