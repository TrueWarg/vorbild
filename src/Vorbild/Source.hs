{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Source (getFilesRecursive)
  where

import System.Directory
  ( listDirectory
  , doesFileExist
  )

import System.FilePath((</>))
import Data.List((\\))
import Control.Monad (filterM)
import qualified Data.Text    as T
import qualified Data.Map.Strict as Map

import Vorbild.TemplateValue.Segment
import Vorbild.TemplateValue.Parsing(PlaceholderSeparator(..), PlaceholderPrefix(..), defaultSeparator, defaultPrefix)

data Source
    = Dir FilePath 
    | FileAndContent T.Text T.Text

generateFromTemplates :: Map.Map TemplateValueId [TemplateValueSegment] -> [Source] -> [Source]
generateFromTemplates _ [] = []
generateFromTemplates values sources = map mapper sources
    where
      mapper source = 
        case source of
          (Dir path) -> Dir path
          (FileAndContent path content) ->
            FileAndContent 
               (placeTemplateValues values path)
               (mconcat $ map (placeTemplateValues values) (T.lines content))

placeTemplateValues :: Map.Map TemplateValueId [TemplateValueSegment] -> T.Text -> T.Text
placeTemplateValues values txt =
    let
        (PlaceholderSeparator separator) = defaultSeparator
        (PlaceholderPrefix prefix) = defaultPrefix
        prefixLength = T.length prefix
        chunks = filter (\chunk -> not $ T.null chunk) (T.splitOn separator txt)
        transform chunk acc = 
          if (T.isPrefixOf prefix chunk)
            then (readValueSegmentList $ values Map.! TemplateValueId (T.drop prefixLength chunk)) <> acc    
            else chunk <> acc
    in
      foldr transform "" chunks

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive dir = do
    content <- listDirectory dir
    (files, subdirs) <- splitOnFilesAndDirs $ fmap (\item -> dir </> item ) content
    case subdirs of
        [] -> pure files
        remainingDirs -> 
            fmap (\other -> other <> files) (foldMap getFilesRecursive remainingDirs)

splitOnFilesAndDirs :: [FilePath] -> IO ([FilePath], [FilePath])
splitOnFilesAndDirs paths = do
    files <- filterM doesFileExist paths
    pure (files, paths \\ files)

