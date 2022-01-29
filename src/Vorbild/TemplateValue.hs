{-# LANGUAGE OverloadedStrings, OverloadedLists, GeneralizedNewtypeDeriving #-}

module Vorbild.TemplateValue
  where

import qualified Data.Text as Text
import Data.List.NonEmpty
import Data.Hashable

data TemplateValueSegment 
    = Single Text.Text
    | Compound  (NonEmpty TemplateValueSegment)
    deriving (Eq, Show)

newtype TemplateValueId = 
    TemplateValueId Text.Text
    deriving (Eq, Hashable, Ord, Show)

instance Semigroup TemplateValueSegment where
    Compound a <> Compound b = Compound (a <> b)
    Single a <> Compound b = Compound ((Single a) <| b)
    Compound a <> Single b = Compound (a <> [(Single b)])
    Single a <> Single b = Compound ([Single a, Single b])

readValueSegment :: TemplateValueSegment -> Text.Text
readValueSegment value = case value of
    (Single txt) -> txt
    (Compound (item :| valueItems)) -> 
        if (null valueItems) then txt
        else txt <>readValueSegment (Compound $ fromList valueItems)
        where txt = readValueSegment item
