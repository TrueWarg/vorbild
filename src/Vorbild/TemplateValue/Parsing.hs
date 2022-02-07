{-# LANGUAGE OverloadedStrings #-}

module Vorbild.TemplateValue.Parsing
  ( PlaceholderSeparator(..)
  , PlaceholderPrefix(..)
  , ModifierSeparator(..)
  , parseValues
  , defaultSeparator
  , defaultPrefix
  ) where

import           Data.List.NonEmpty             (fromList)
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import           Vorbild.TemplateValue.Config
import           Vorbild.TemplateValue.Modifier (Modifier, tryParseModifier)
import           Vorbild.TemplateValue.Segment

newtype PlaceholderSeparator =
  PlaceholderSeparator T.Text

newtype PlaceholderPrefix =
  PlaceholderPrefix T.Text

newtype ModifierSeparator =
  ModifierSeparator T.Text

defaultSeparator = PlaceholderSeparator "~"

defaultPrefix = PlaceholderPrefix "^"

defaultMofifierSeparator = ModifierSeparator "#"

data Token
  = Const T.Text
  | Value [Modifier] T.Text
  deriving Show

parseValues ::
     Map.Map ValueName RawValue
  -> Map.Map TemplateValueId [TemplateValueSegment]
parseValues raws =
  if (Map.null raws)
    then Map.empty
    else Map.mapKeys TemplateValueId (Map.map valuesMapper raws)
  where
    valuesMapper raw =
      let tokens = extractTokens raw
          tokensMapper token =
            case token of
              Const txt -> Single txt
              Value modifiers name ->
                Compound modifiers $ fromList (valuesMapper (raws Map.! name))
       in map tokensMapper tokens

-- todo: use Reader to extract some Token Value detector and extractor
-- curently const~^ValueName#modifier1#modifier2~const
extractTokens :: T.Text -> [Token]
extractTokens "" = [Const ""]
extractTokens line =
  let (PlaceholderSeparator separator) = defaultSeparator
      (PlaceholderPrefix prefix) = defaultPrefix
      (ModifierSeparator modifierSeparator) = defaultMofifierSeparator
      prefixLength = T.length prefix
      splitted = filter (\txt -> not $ T.null txt) (T.splitOn separator line)
      constructValue txt =
        let statement = T.drop prefixLength txt
            (valueName, modifiersBlock) = T.breakOn modifierSeparator statement
            modifiers = parseModifiers modifiersBlock
         in Value modifiers valueName
      transform =
        (\txt ->
           if (T.isPrefixOf prefix txt)
             then constructValue txt
             else Const txt)
   in map transform splitted

parseModifiers :: T.Text -> [Modifier]
parseModifiers modifiersBlock =
  let (ModifierSeparator separator) = defaultMofifierSeparator
      dropFirstSeparator block = T.drop (T.length separator) block
      modifiersCodes = T.splitOn separator (dropFirstSeparator modifiersBlock)
      maybeModifiers = map tryParseModifier modifiersCodes
      onlyParsed = filter (\item -> item /= Nothing) maybeModifiers
   in map (\(Just modifier) -> modifier) onlyParsed
