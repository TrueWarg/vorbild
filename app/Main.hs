module Main where
    
import Command.Parsers
import Command.Options

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
    option <- parse
    putStrLn $ templatePath option
    putStrLn $ show $ destination option
    pure ()

