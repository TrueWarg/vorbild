module Main where

import           Command.Options
import           Command.Parsers

import           System.FilePath  (takeBaseName, takeExtension, takeFileName,
                                   (<.>), (</>))

import           System.Directory (getCurrentDirectory)
import           System.IO

import           Vorbild

main :: IO ()
main = do
  option <- parse
  putStrLn $ templatePath option
  putStrLn $ show $ destination option
  pure ()
