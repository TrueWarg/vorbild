{-# LANGUAGE OverloadedStrings #-}

module Vorbild.TemplateValue.Parsing
  ( parseValues
  ) where

import           Data.List.NonEmpty             (fromList)
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import           Vorbild.TemplateValue.Config   (PlaceholderConfig (..),
                                                 RawValue, ValueName)
import           Vorbild.TemplateValue.Modifier (Modifier, tryParseModifier)
import           Vorbild.TemplateValue.Segment
import           Vorbild.Text(splitOnAnyOf)

data Token
  = Const T.Text
  | Value [Modifier] T.Text
  deriving (Show)

parseValues ::
     PlaceholderConfig
  -> Map.Map ValueName RawValue
  -> Map.Map TemplateValueId [TemplateValueSegment]
parseValues config raws =
  if (Map.null raws)
    then Map.empty
    else Map.mapKeys TemplateValueId (Map.map valuesMapper raws)
  where
    valuesMapper raw =
      let tokens = extractTokens config raw
          tokensMapper token =
            case token of
              Const txt -> Single txt
              Value modifiers name ->
                Compound modifiers $ fromList (valuesMapper (raws Map.! name))
       in map tokensMapper tokens

-- todo: use MonadReader to extract some Token Value detector and extractor
-- curently const{{^ValueName#modifier1#modifier2}}const
extractTokens :: PlaceholderConfig -> T.Text -> [Token]
extractTokens _ "" = [Const ""]
extractTokens config line =
  let oTeg = openTag config
      cTeg = closeTag config
      prefix = valuePrefix config
      separator = modifierSeparator config
      prefixLength = T.length prefix
      splitted =
        filter (\txt -> not $ T.null txt) (splitOnAnyOf [oTeg, cTeg] line)
      constructValue txt =
        let statement = T.drop prefixLength txt
            (valueName, modifiersBlock) = T.breakOn separator statement
            modifiers = parseModifiers separator modifiersBlock
         in Value modifiers valueName
      transform =
        (\txt ->
           if (T.isPrefixOf prefix txt)
             then constructValue txt
             else Const txt)
   in map transform splitted

parseModifiers :: T.Text -> T.Text -> [Modifier]
parseModifiers separator modifiersBlock =
  let dropFirstSeparator block = T.drop (T.length separator) block
      modifiersCodes = T.splitOn separator (dropFirstSeparator modifiersBlock)
      maybeModifiers = map tryParseModifier modifiersCodes
      onlyParsed = filter (\item -> item /= Nothing) maybeModifiers
   in map (\(Just modifier) -> modifier) onlyParsed
