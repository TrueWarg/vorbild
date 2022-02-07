module Vorbild
  ( readAndParseConfigItemsFromJson
  , prepareRawValues
  , readAndParsePlaceholderConfigFromJson
  , ValueConfigItem(..)
  , PlaceholderConfig(..)
  , parseValues
  , readValueSegment
  , SourceAndContent(..)
  , Source(..)
  , generateFromTemplates
  , placeTemplateValues
  , getSourcesRecursive
  , toSourceAndContent
  ) where

import           Vorbild.Source
import           Vorbild.TemplateValue.Config  (PlaceholderConfig (..),
                                                ValueConfigItem (..),
                                                prepareRawValues,
                                                readAndParseConfigItemsFromJson,
                                                readAndParsePlaceholderConfigFromJson)

import           Vorbild.TemplateValue.Parsing (parseValues)
import           Vorbild.TemplateValue.Segment (readValueSegment)
