-- todo: reorganize modules to avoid names conflicts
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
  , Action(..)
  , Edges(..)
  , ModificationError(..)
  , ValuesAndConfig(..)
  , SegmentParsingError(..)
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
  , getFiles
  , splitOnFilesAndDirs
  , execModificationsIO
  , execModifications
  , mapBlockConfigList
  ) where

import           Vorbild.File
import           Vorbild.Modifieble.Block
import           Vorbild.Modifieble.Config
import           Vorbild.Modifieble.ExecIO
import           Vorbild.Modifieble.Exec
import           Vorbild.Modifieble.Mapper
import           Vorbild.Source
import           Vorbild.TemplateValue.Config
import           Vorbild.TemplateValue.Modifier
import           Vorbild.TemplateValue.Parsing
import           Vorbild.TemplateValue.Placement
import           Vorbild.TemplateValue.Segment
