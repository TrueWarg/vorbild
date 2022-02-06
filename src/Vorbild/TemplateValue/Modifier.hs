{-# LANGUAGE OverloadedStrings #-}

module Vorbild.TemplateValue.Modifier
  ( Modifier
  , applyModifier
  , tryParseModifier
  ) where

import qualified Data.Text as T

data Modifier
  = Replace T.Text T.Text
  | ToLower
  deriving (Eq, Show)

applyModifier :: T.Text -> Modifier -> T.Text
applyModifier txt modifier =
  case modifier of
    (Replace old new) -> applyReplace txt old new
    ToLower           -> T.toLower txt

applyReplace txt old new =
  if (txt == "")
    then txt
    else T.replace old new txt

tryParseModifier :: T.Text -> Maybe Modifier
tryParseModifier txt = parse $ T.strip txt
  where
    parse statement
      | T.isPrefixOf replaceCode statement = tryParceReplace statement
      | statement == toLowerCode = Just ToLower
      | otherwise = Nothing

tryParceReplace statement =
  let argsBlock = T.drop (T.length replaceCode) statement
      args = map (\arg -> T.strip arg) (T.splitOn " " argsBlock)
      filterEmpty args = filter (not . T.null) args
   in case (filterEmpty args) of
        old:new:[] -> Just $ Replace (parseArg old) (parseArg new)
        otherwise  -> Nothing

parseArg :: T.Text -> T.Text
parseArg arg
  | T.head arg == '\'' && T.last arg == '\'' = T.dropAround (\ch -> ch == '\'') arg
  | otherwise = arg

replaceCode = "replace"

toLowerCode = "toLower"
