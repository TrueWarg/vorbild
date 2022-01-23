module Main where

import Command.Options
import Command.Parsers

import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )

import System.Directory (getCurrentDirectory)
import System.IO

import Vorbild

main :: IO ()
main = do
    options <- parse
    case options of
        FromTempatePath templatePath destination -> do
            putStrLn "test create from path"
            content <- readFile templatePath
            targetDir <- case destination of
                NoSpec -> getCurrentDirectory
                Dir path -> pure path
            writeFile (targetDir </> takeFileName templatePath) content

        FromTemplateName name templatesSrc -> do
            putStrLn "test create from name" 
            fieldsPath <- fmap (\dir -> dir </> "vorbild-templates" </> name </> "Fields.txt") getCurrentDirectory
            fields <- Vorbild.readAndParseFields fieldsPath
            filledFields <- Vorbild.fillFields fields
            putStrLn $ show filledFields

