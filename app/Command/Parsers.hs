module Command.Parsers
  ( parse
  ) where

import           Data.Maybe          (fromMaybe)
import           Options.Applicative

import           Command.Options

parse :: IO Option
parse = execParser opts
  where
    opts =
      info
        (helper <*> optionParser)
        (fullDesc <>
         header "VorbilD" <> progDesc "Generate files from specified templates")

optionParser :: Parser Option
optionParser = Option <$> templatePathParser <*> destinationParser

destinationParser :: Parser Destination
destinationParser = fromMaybe NoSpec <$> optional (fmap Dir parser)
  where
    parser =
      strOption
        (long "dst" <>
         short 'd' <>
         metavar "destination path" <>
         showDefault <>
         help
           "Destination path of generated file. Current dir will be used if it isn't specified")

templatePathParser :: Parser FilePath
templatePathParser =
  strOption
    (long "src" <>
     short 's' <> metavar "template path" <> help "Input template source dir")
