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
  valueConfigPath <- correctFile $ templatePath </> valueConfigName
  templatesSourcePath <- correctDir $ templatePath </> templateSourceDir
  valueConfigItems <- tryReadAndParseConfigItemsFromJson valueConfigPath
  placeholderConfig <- tryReadPlaceholderConfigOrDefault templatePath
  modifiebleFiles <- tryReadModifiebleConfigFilesOrEmpty $ templatePath </> modifiebleSourceDir
  modifiebleConfigs <- tryReadAndParseModifiebleConfigsFromJson modifiebleFiles
  values <- tryPrepareAndParseValues placeholderConfig valueConfigItems
  putStrLn "Processing..."
  sources <- getSourcesRecursive templatesSourcePath >>= toSourceAndContent
  generated <- tryGenerateFromTemplates placeholderConfig values sources
  let replaceRoot' txtPath =
        replaceRoot templatesSourcePath destination (T.unpack txtPath)
      writeFiles src =
        case src of
          Vorbild.Dir path -> createDirectory (replaceRoot' path)
          FileAndContent path content ->
            createAndWriteFile (replaceRoot' path) content
  _ <- traverse writeFiles generated
  execModification modifiebleConfigs
  putStrLn "Done"
  pure ()

class Format a where
  format :: a -> String

instance Format ConfigParsingError where
  format (ConfigParsingError cause srcPath) =
    "File parsing error " <> srcPath <> ": " <> cause

instance Format ValueParsingError where
  format (UnkonwnName valueName) = "Unknow value with name: " <> valueName
  format (CycleDeclaration valueName) = "Cycle declaration value with name: " <> valueName

instance Format InTmpValueParsingError where
  format (InTmpValueParsingError valueName path) =
    "Unknow value with name: " <>
    valueName <> " in template path: " <> path

instance Format ModifiebleParsingError where
  format (ModifiebleParsingError cause srcPath) =
    "File parsing error " <> srcPath <> ": " <> cause

successOrPutError :: Format e => IO (Either e s) -> IO s
successOrPutError action = do
  result <- action
  case result of
    Left e  -> stderrAndExit $ format e
    Right s -> pure s

tryGenerateFromTemplates config values sources =
  successOrPutError $ pure (generateFromTemplates config values sources)

tryPrepareAndParseValues config items =
  successOrPutError $ fmap (parseValues config) (prepareRawValues items)

tryReadAndParseConfigItemsFromJson =
  successOrPutError . readAndParseConfigItemsFromJson

tryReadAndParsePlaceholderConfigFromJson =
  successOrPutError . readAndParsePlaceholderConfigFromJson

tryReadAndParseModifiebleConfigsFromJson =
  successOrPutError . readAndParseModifiebleConfigsFromJson

tryReadModifiebleConfigFilesOrEmpty :: FilePath -> IO [FilePath]
tryReadModifiebleConfigFilesOrEmpty dir = do
  isExist <- doesDirectoryExist dir
  files <- if (isExist) then getFiles dir else pure []
  pure $ fmap (\item -> dir </> item) files

correctDir :: FilePath -> IO FilePath
correctDir path = do
  isExist <- doesDirectoryExist path
  if (isExist)
    then pure path
    else stderrAndExit ("Dir " <> path <> " is not exist")

correctFile :: FilePath -> IO FilePath
correctFile path = do
  isExist <- doesFileExist path
  if (isExist)
    then pure path
    else stderrAndExit ("File " <> path <> " is not exist")

createAndWriteFile :: FilePath -> T.Text -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  T.writeFile path content

stderrAndExit txt = hPutStrLn stderr txt *> exitFailure

tryReadPlaceholderConfigOrDefault :: FilePath -> IO PlaceholderConfig
tryReadPlaceholderConfigOrDefault templatePath = do
  let fullPath = templatePath </> placeholderConfigName
      useDefaultMessage =
        placeholderConfigName <>
        " is not found, use default: " <> show defaultPlaceholderConfig
  iIsExist <- doesFileExist fullPath
  if (iIsExist)
    then putStrLn "Use castom placeholder config" *>
         tryReadAndParsePlaceholderConfigFromJson fullPath
    else putStrLn useDefaultMessage *> pure defaultPlaceholderConfig

replaceRoot :: FilePath -> FilePath -> FilePath -> FilePath
replaceRoot root newRoot path = newRoot </> makeRelative root path

execModification :: [ModifiebleFile] -> IO ()
execModification configs = pure ()

valueConfigName = "values.json"

placeholderConfigName = "placeholder.json"

templateSourceDir = "source"

modifiebleSourceDir = "modifieble"

defaultPlaceholderConfig =
  PlaceholderConfig
    { openTag = "{{"
    , closeTag = "}}"
    , valuePrefix = "^"
    , modifierSeparator = "#"
    }
