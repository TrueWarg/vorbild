{-# LANGUAGE OverloadedStrings #-}

module Vorbild.TemplateValue.Modifier
  ( Modifier(..)
  , applyModifier
  , applyModifiers
  , tryParseModifier
  ) where

import qualified Data.Text    as T

import           Vorbild.Text (toCamelCase, toKebabCase, toSnakeCase)

data Modifier
  = Replace T.Text T.Text
  | ToLower
  | ToUpper
  | ToTitle
  | ToCaseFold
  | ToCamel
  | ToSnake
  | ToKebab
  deriving (Eq, Show)

applyModifier :: T.Text -> Modifier -> T.Text
applyModifier txt modifier =
  case modifier of
    (Replace old new) -> applyReplace txt old new
    ToLower           -> T.toLower txt
    ToUpper           -> T.toUpper txt
    ToTitle           -> T.toTitle txt
    ToCaseFold        -> T.toCaseFold txt
    ToCamel           -> toCamelCase txt
    ToSnake           -> toSnakeCase txt
    ToKebab           -> toKebabCase txt

applyModifiers :: T.Text -> [Modifier] -> T.Text
applyModifiers txt [] = txt
applyModifiers txt (modifier:remaining) =
  applyModifiers (applyModifier txt modifier) remaining

applyReplace txt old new =
  if (txt == "")
    then txt
    else T.replace old new txt

tryParseModifier :: T.Text -> Maybe Modifier
tryParseModifier txt = parse $ T.strip txt
  where
    parse statement
      | T.isPrefixOf replaceCode statement = tryParseReplace statement
      | statement == "toLower" = Just ToLower
      | statement == "toUpper" = Just ToUpper
      | statement == "capitalize" = Just ToTitle
      | statement == "toCaseFold" = Just ToCaseFold
      | statement == "toCamel" = Just ToCamel
      | statement == "toSnake" = Just ToSnake
      | statement == "toKebab" = Just ToKebab
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
