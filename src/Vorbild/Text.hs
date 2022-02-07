module Vorbild.Text
  ( splitOnAnyOf
  ) where

import qualified Data.Text as T

splitOnAnyOf :: [T.Text] -> T.Text -> [T.Text]
splitOnAnyOf separators txt =
  foldl (\acc separator -> acc >>= T.splitOn separator) [txt] separators
