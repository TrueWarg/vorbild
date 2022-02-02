{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Vorbild.TemplateValue.Segment where

import           Data.Hashable
import           Data.List.NonEmpty as NonEmpty
import qualified Data.Text          as Text

data TemplateValueSegment
  = Single Text.Text
  | Compound (NonEmpty TemplateValueSegment)
  deriving (Eq, Show)

newtype TemplateValueId =
  TemplateValueId Text.Text
  deriving (Eq, Hashable, Ord, Show)

instance Semigroup TemplateValueSegment where
  Compound a <> Compound b = Compound (a <> b)
  Single a <> Compound b   = Compound ((Single a) <| b)
  Compound a <> Single b   = Compound (a <> NonEmpty.fromList [(Single b)])
  Single a <> Single b     = Compound (NonEmpty.fromList [Single a, Single b])

readValueSegment :: TemplateValueSegment -> Text.Text
readValueSegment segment =
  case segment of
    (Single txt) -> txt
    (Compound (item :| segments)) ->
      if (null segments)
        then txt
        else txt <> readValueSegment (Compound $ fromList segments)
      where txt = readValueSegment item

readValueSegmentList :: [TemplateValueSegment] -> Text.Text
readValueSegmentList segments = mconcat (Prelude.map readValueSegment segments)
