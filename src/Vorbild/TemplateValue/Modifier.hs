{-# LANGUAGE OverloadedStrings #-}

module Vorbild.TemplateValue.Modifier
  ( Modifier(..)
  , applyModifier
  , applyModifiers
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

applyModifiers :: T.Text -> [Modifier] -> T.Text
applyModifiers txt []        = txt
applyModifiers txt (modifier : remaining) = applyModifiers (applyModifier txt modifier) remaining

applyReplace txt old new =
  if (txt == "")
    then txt
    else T.replace old new txt

tryParseModifier :: T.Text -> Maybe Modifier
tryParseModifier txt = parse $ T.strip txt
  where
    parse statement
      | T.isPrefixOf replaceCode statement = tryParseReplace statement
      | statement == toLowerCode = Just ToLower
      | otherwise = Nothing

tryParseReplace statement =
  let argsBlock = T.drop (T.length replaceCode) statement
      args = map (\arg -> T.strip arg) (T.splitOn " " argsBlock)
      filterEmpty args' = filter (not . T.null) args'
   in case (filterEmpty args) of
        old:new:[] -> Just $ Replace (parseReplaceArg old) (parseReplaceArg new)
        _          -> Nothing

parseReplaceArg :: T.Text -> T.Text
parseReplaceArg arg
  | T.head arg == '\'' && T.last arg == '\'' =
    T.dropAround (\ch -> ch == '\'') arg
  | otherwise = arg

replaceCode = "replace"

toLowerCode = "toLower"
