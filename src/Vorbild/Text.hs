{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Text where

import qualified Data.Text as T

splitOnAnyOf :: [T.Text] -> T.Text -> [T.Text]
splitOnAnyOf separators txt =
  foldl (\acc separator -> acc >>= T.splitOn separator) [txt] separators

data BreakOnThreeError
  = EmptyArg
  | Fail
  deriving (Show, Eq)

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
  if (T.null beforeStartIncl || T.null afterStart)
    then Left Fail
    else Right (beforeStartIncl, body, afterEndIncl)
  where
    (beforeStartIncl, afterStart) = T.breakOnEnd start txt
    (body, afterEndIncl) = T.breakOn end afterStart
