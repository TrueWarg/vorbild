module Vorbild.File where

import           Control.Monad    (filterM)
import           Data.List        ((\\))
import           System.Directory (doesFileExist, listDirectory)
import           System.FilePath                ((</>))

splitOnFilesAndDirs :: FilePath -> IO ([FilePath], [FilePath])
splitOnFilesAndDirs dir = do
  content <- listDirectory dir
  let
    paths = fmap (\item -> dir </> item) content
  files <- filterM doesFileExist paths
  pure (files, paths \\ files)

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  content <- listDirectory dir
  filterM doesFileExist $ fmap (\item -> dir </> item) content
