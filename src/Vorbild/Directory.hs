module Vorbild.Directory (getFilesRecursive)
  where

import System.Directory
  ( listDirectory
  , doesFileExist
  )

import System.FilePath((</>))

import Data.List((\\))

import Control.Monad (filterM)

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