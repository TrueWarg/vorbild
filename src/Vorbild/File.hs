module Vorbild.File where

import           Control.Monad    (filterM)
import           Data.List        ((\\))
import           System.Directory (doesFileExist, listDirectory)

splitOnFilesAndDirs :: [FilePath] -> IO ([FilePath], [FilePath])
splitOnFilesAndDirs paths = do
  files <- filterM doesFileExist paths
  pure (files, paths \\ files)

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  content <- listDirectory dir
  filterM doesFileExist content
