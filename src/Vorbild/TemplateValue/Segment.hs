{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Vorbild.TemplateValue.Segment where

import           Data.Hashable
import           Data.List.NonEmpty             as NonEmpty
import qualified Data.Text                      as T
import           Vorbild.TemplateValue.Modifier (Modifier, applyModifiers)

data TemplateValueSegment
  = Single T.Text
  | Compound [Modifier] (NonEmpty TemplateValueSegment)
  deriving (Eq, Show)

newtype TemplateValueId =
  TemplateValueId T.Text
  deriving (Eq, Hashable, Ord, Show)
  
readValueSegment :: TemplateValueSegment -> T.Text
readValueSegment segment =
  case segment of
    (Single txt) -> txt
    (Compound modifiers (item :| segments)) -> applyModifiers txt modifiers
      where txt =
              if (null segments)
                then readValueSegment item
                else (readValueSegment item) <>
                     readValueSegment (Compound [] $ fromList segments)

readValueSegmentList :: [TemplateValueSegment] -> T.Text
readValueSegmentList segments = mconcat (Prelude.map readValueSegment segments)
