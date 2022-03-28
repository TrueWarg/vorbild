{-# LANGUAGE OverloadedStrings #-}

module Vorbild.TemplateValue.Placement
  ( placeTemplateValues
  , ValuesAndConfig(..)
  ) where

import           Control.Monad.Reader           (Reader, ask)
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import           Vorbild.Either
import           Vorbild.TemplateValue.Config
import           Vorbild.TemplateValue.Modifier (Modifier (..), applyModifiers,
                                                 tryParseModifier)
import           Vorbild.TemplateValue.Segment
import           Vorbild.Text                   (splitOnAnyOf)

data ValuesAndConfig =
  ValuesAndConfig
    (Map.Map TemplateValueId [TemplateValueSegment])
    PlaceholderConfig
  deriving (Show)

-- There is idea to represent all text as TemplateSegment to easy modify text
-- and for better generalisation
-- But need to determinate start of recursive calculation.
-- Currently this start func placeTemplateValues.
-- todo: research this idea (it's realy needed? How to impliment it better? etc.)
placeTemplateValues :: T.Text -> Reader ValuesAndConfig (Either T.Text T.Text)
placeTemplateValues txt = do
  ValuesAndConfig values config <- ask
  let oTeg = openTag config
      cTeg = closeTag config
      prefix = valuePrefix config
      separator = modifierSeparator config
      statement chunk = T.drop (T.length prefix) chunk
      chunks =
        filter (\chunk -> not $ T.null chunk) (splitOnAnyOf [oTeg, cTeg] txt)
      parseSegments chunk =
        let (valueName, modifiersBlock) = T.breakOn separator (statement chunk)
            inSourceModifiers = parseModifiers separator modifiersBlock
         in case (Map.lookup (TemplateValueId valueName) values) of
              Just segments ->
                Right $
                applyModifiers (readValueSegmentList segments) inSourceModifiers
              Nothing -> Left valueName
      transform acc chunk =
        if (T.isPrefixOf prefix chunk)
          then accumulate acc (parseSegments chunk)
          else accumulate acc (Right chunk)
      results = foldl transform (Right "") chunks
  pure $ results

-- todo: add error for parseModifiers?
parseModifiers :: T.Text -> T.Text -> [Modifier]
parseModifiers separator modifiersBlock =
  let dropFirstSeparator block = T.drop (T.length separator) block
      modifiersCodes = T.splitOn separator (dropFirstSeparator modifiersBlock)
      maybeModifiers = map tryParseModifier modifiersCodes
      onlyParsed = filter (\item -> item /= Nothing) maybeModifiers
   in map (\(Just modifier) -> modifier) onlyParsed
