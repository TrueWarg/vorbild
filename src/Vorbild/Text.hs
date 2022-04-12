{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Text where

import qualified Data.Char as Ch
import qualified Data.Text as T

data BreakOnThreeError
  = EmptyArg
  | Fail
  deriving (Show, Eq)

splitOnAnyOf :: [T.Text] -> T.Text -> [T.Text]
splitOnAnyOf separators txt =
  foldl (\acc separator -> acc >>= T.splitOn separator) [txt] separators

splitByTagsInclude :: T.Text -> T.Text -> T.Text -> [T.Text]
splitByTagsInclude _ _ "" = []
splitByTagsInclude start end txt =
  let (beforeStart, afterStartIncl) = T.breakOn start txt
      (beforEnd, afterEndIncl) = T.breakOn end afterStartIncl
      beforEndIncl =
        if (T.null afterEndIncl)
          then beforEnd
          else beforEnd <> end
      afterEnd = T.drop (T.length end) afterEndIncl
   in beforeStart : beforEndIncl : splitByTagsInclude start end afterEnd

-- OK
-- "aaaaaaaaSTARTaaaaaaaaENDaaa"
-- "aaaaaaaaSTART", "aaaaaaaa", "ENDaaa"
-- BAD
-- "aaaaaaaaSTARTaaaaaaaaaaaaaa"
-- "aaaaaaaaSTART", "", "aaaaaaaaaaaaaa"
-- BAD
-- "aaaaaaaaENDaaaaaaaaaaaaaa"
-- "", "aaaaaaaa", "ENDaaaaaaaaaaaaaa"
-- BAD
-- "aaaaaaaaaaaaaaaaaaaaaaaaa"
-- "", "aaaaaaaaaaaaaaaaaaaaaaaaa", "", "aaaaaaaaaaaaaaaaaaaaaaaaa"
breakOnThree ::
     T.Text
  -> T.Text
  -> T.Text
  -> Either BreakOnThreeError (T.Text, T.Text, T.Text)
breakOnThree "" _ _ = Left EmptyArg
breakOnThree _ "" _ = Left EmptyArg
breakOnThree start end txt =
  if (T.null beforeStartIncl || T.null afterStart || T.null afterEndIncl)
    then Left Fail
    else Right (beforeStartIncl, body, afterEndIncl)
  where
    (beforeStart, afterStartIncl) = T.breakOn start txt
    beforeStartIncl = beforeStart <> start
    afterStart = T.drop (T.length start) afterStartIncl
    (body, afterEndIncl) = T.breakOn end afterStart

-- todo find default function or impl correclty
isSubText :: T.Text -> T.Text -> Bool
isSubText query txt =
  let (_, after) = T.breakOn query txt
   in not $ T.null after

toCamelCase :: T.Text -> T.Text
toCamelCase txt = T.concat $ first : map (T.toTitle) remaining
  where
    first:remaining = splitOnAnyOf ["-", "_"] txt

toSnakeCase :: T.Text -> T.Text
toSnakeCase txt = T.pack $ toCamelAntogonistCase '-' '_' (T.unpack txt)

toKebabCase :: T.Text -> T.Text
toKebabCase txt = T.pack $ toCamelAntogonistCase '_' '-' (T.unpack txt)

toCamelAntogonistCase :: Char -> Char -> String -> String
toCamelAntogonistCase separator coSeparator = toCamelAntogonistCase' []
  where
    handle currentWord prev curr remaining
      | prev == separator && curr == separator =
        currentWord <>
        (coSeparator : coSeparator : (toCamelAntogonistCase' [] remaining))
      | prev == separator =
        currentWord <>
        (coSeparator : curr : (toCamelAntogonistCase' [] remaining))
      | curr == separator =
        currentWord <>
        (prev : coSeparator : (toCamelAntogonistCase' [] remaining))
      | Ch.isLower prev && Ch.isUpper curr =
        currentWord <>
        (prev :
         coSeparator : (Ch.toLower curr) : (toCamelAntogonistCase' [] remaining))
      | otherwise =
        toCamelAntogonistCase' (currentWord <> [prev]) (curr : remaining)
    toCamelAntogonistCase' currentWord txt =
      case txt of
        []                  -> currentWord
        prev:curr:remaining -> handle currentWord prev curr remaining
        [_]                 -> currentWord <> txt
