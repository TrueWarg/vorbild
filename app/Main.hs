{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Command.Options  as Options
import           Command.Parsers

import           System.FilePath  (makeRelative, takeDirectory, (</>))

import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           System.Directory (createDirectory, createDirectoryIfMissing,
                                   doesDirectoryExist, doesFileExist,
                                   getCurrentDirectory)
import           System.Exit      (exitFailure)
import           System.IO

import           Vorbild

main :: IO ()
main = do
  option <- parse
  templatePath <- correctDir $ Options.templatePath option
  destination <-
    case Options.destination option of
      NoSpec           -> getCurrentDirectory
      Options.Dir path -> correctDir path
  valueConfigPath <- correctFile $ templatePath </> defaultValueConfigName
  templatesSourcePath <- correctDir $ templatePath </> defaultTemplateSourceDir
  valueConfigItems <- readAndParseConfigItemsFromJson valueConfigPath
  values <- fmap parseValues (prepareRawValues valueConfigItems)
  putStrLn "Processing..."
  sources <- getSourcesRecursive templatesSourcePath >>= toSourceAndContent
  let rootReplaces txtPath =
        replaceRoot templatesSourcePath destination (T.unpack txtPath)
      generated = generateFromTemplates values sources
      writeFiles src =
        case src of
          Vorbild.Dir path -> createDirectory (rootReplaces path)
          FileAndContent path content ->
            createAndWriteFile (rootReplaces path) content
  _ <- traverse writeFiles generated
  putStrLn "Done"
  pure ()

correctDir :: FilePath -> IO FilePath
correctDir path = do
  isExist <- doesDirectoryExist path
  if (isExist)
    then pure path
    else (hPutStrLn stderr ("Dir " <> path <> " is not exist") *> exitFailure)

correctFile :: FilePath -> IO FilePath
correctFile path = do
  isExist <- doesFileExist path
  if (isExist)
    then pure path
    else (hPutStrLn stderr ("File " <> path <> " is not exist") *> exitFailure)

createAndWriteFile :: FilePath -> T.Text -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  T.writeFile path content

replaceRoot :: FilePath -> FilePath -> FilePath -> FilePath
replaceRoot root newRoot path = newRoot </> makeRelative root path

defaultValueConfigName = "values.json"

defaultTemplateSourceDir = "source"
