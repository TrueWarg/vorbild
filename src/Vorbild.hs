module Vorbild
  ( ValueConfigItem(..)
  , PlaceholderConfig(..)
  , ConfigParsingError(..)
  , ValueParsingError(..)
  , InTmpValueParsingError(..)
  , ValueName
  , RawValue
  , TemplateValueSegment(..)
  , TemplateValueId(..)
  , Modifier(..)
  , SourceAndContent(..)
  , Source(..)
  , ModifiebleFile(..)
  , BlockDescriptorItem(..)
  , ModifiebleParsingError(..)
  , Descriptor(..)
  , DescriptorId(..)
  , Action(..)
  , modify
  , readAndParseModifiebleConfigsFromJson
  , readAndParseConfigItemsFromJson
  , prepareRawValues
  , readAndParsePlaceholderConfigFromJson
  , parseValues
  , readValueSegment
  , generateFromTemplates
  , getSourcesRecursive
  , toSourceAndContent
  ) where

import           Vorbild.Source
import           Vorbild.Modifieble.Block
import           Vorbild.Modifieble.Config
import           Vorbild.TemplateValue.Config
import           Vorbild.TemplateValue.Modifier
import           Vorbild.TemplateValue.Parsing
import           Vorbild.TemplateValue.Segment
