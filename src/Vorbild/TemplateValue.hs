{-# LANGUAGE OverloadedStrings #-}

module Vorbild.TemplateValue
  where

import qualified Data.Text as Text
import Data.List.NonEmpty


data TemplateValue 
    = Single Text.Text
    | Compound  (NonEmpty TemplateValue)
    deriving Show

newtype TemplateValueId = 
    TemplateValueId Text.Text
    deriving Show

instance Semigroup TemplateValue where
    Compound a <> Compound b = Compound (a <> b)
    Single a <> Compound b = Compound ((Single a) <| b)
    Compound a <> Single b = Compound (a <> fromList [Single b])
    Single a <> Single b = Compound (fromList [Single a, Single b])

readValue :: TemplateValue -> Text.Text
readValue value = case value of
    (Single txt) -> txt
    (Compound (item :| valueItems)) -> 
        if (null valueItems) then readValue item
        else readValue (Compound $ fromList valueItems)

