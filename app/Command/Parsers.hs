module Command.Parsers
  (parse)
  where

import Data.Maybe (fromMaybe)
import Options.Applicative

import Command.Options

parse :: IO Option
parse = execParser opts
    where
        opts = 
            info (helper <*> mainParser)
            (
                fullDesc
                <> header "Templater"
                <> progDesc "Generate files from specified templates"
            )

mainParser :: Parser Option
mainParser =
    subparser
       (
          command
          "frompath"
           (
            info
                (helper <*> fromTemplatePathParser)
                (progDesc "Generate file from template which specified in certain path")
            )
            <>
            command
            "fromname"
            (
            info
                (helper <*> fromTemplateNameParser)
                (progDesc "Generate file from template name that vorbild will try to find in <currentDir>/vorbild_templates")
            )
       )

fromTemplatePathParser :: Parser Option
fromTemplatePathParser =
    FromTempatePath <$> templatePathParser <*> destinationParser

templatePathParser :: Parser FilePath
templatePathParser =
    strOption
        (
            long "temp"
            <> short 't'
            <> metavar "TEMPALTE DIRECTORY"
            <> help "input template deirectory"
        )

destinationParser :: Parser Source
destinationParser = 
    fromMaybe NoSpec <$> optional (fmap Dir parser)
    where
        parser = strOption
                    (
                        long "dst"
                        <> short 'd'
                        <> metavar "DESTINATION"
                        <> help "destination path"
                    )

fromTemplateNameParser :: Parser Option
fromTemplateNameParser =
    FromTemplateName <$> templateNameParser <*> tempatesSourceParser

templateNameParser :: Parser FilePath
templateNameParser =
    strOption
        (
            long "name"
            <> short 'n'
            <> metavar "TEMPALTE NAME"
            <> help "input template name"
        )

tempatesSourceParser :: Parser Source
tempatesSourceParser = 
    fromMaybe NoSpec <$> optional (fmap Dir parser)
    where
        parser = strOption
                    (
                        long "tsrc"
                        <> short 's'
                        <> metavar "TEMPLATE SRC"
                        <> help "template source dir"
                    )