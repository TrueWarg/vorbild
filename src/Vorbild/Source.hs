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

import           Vorbild.TemplateValue.Parsing (PlaceholderPrefix (..),
                                                PlaceholderSeparator (..),
                                                defaultPrefix, defaultSeparator)

import           Vorbild.TemplateValue.Segment

data SourceAndContent
  = Dir T.Text
  | FileAndContent T.Text T.Text

data Source
  = SourceDir FilePath
  | SourceFile FilePath

generateFromTemplates ::
     Map.Map TemplateValueId [TemplateValueSegment]
  -> [SourceAndContent]
  -> [SourceAndContent]
generateFromTemplates _ [] = []
generateFromTemplates values sources = map mapper sources
  where
    mapper source =
      case source of
        (Dir path) -> Dir $ placeTemplateValues values path
        (FileAndContent path content) ->
          FileAndContent
            (placeTemplateValues values path)
            (mconcat $ map (placeTemplateValues values) (T.lines content))

placeTemplateValues ::
     Map.Map TemplateValueId [TemplateValueSegment] -> T.Text -> T.Text
placeTemplateValues values txt =
  let (PlaceholderSeparator separator) = defaultSeparator
      (PlaceholderPrefix prefix) = defaultPrefix
      prefixLength = T.length prefix
      chunks = filter (\chunk -> not $ T.null chunk) (T.splitOn separator txt)
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
