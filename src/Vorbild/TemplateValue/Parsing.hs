{-# LANGUAGE OverloadedStrings #-}

module Vorbild.TemplateValue.Parsing
  ( parseValues
  , ValueParsingError(..)
  ) where

import           Data.List.NonEmpty             (fromList)
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import           Vorbild.TemplateValue.Config   (PlaceholderConfig (..),
                                                 RawValue, ValueName)
import           Vorbild.TemplateValue.Modifier (Modifier, tryParseModifier)
import           Vorbild.TemplateValue.Segment
import           Vorbild.Text                   (splitOnAnyOf)

data Token
  = Const T.Text
  | Value [Modifier] T.Text
  deriving (Show)

data ValueParsingError
  = UnkonwnName String
  | CycleDeclaration String
  deriving (Eq, Show)

parseValues ::
     PlaceholderConfig
  -> Map.Map ValueName RawValue
  -> Either ValueParsingError (Map.Map TemplateValueId [TemplateValueSegment])
parseValues config raws
  | Map.null raws = Right Map.empty
  | Map.null errors = Right $ Map.mapKeys TemplateValueId succeses
  | otherwise = Left $ head $ Map.elems errors
  where
    valuesMapper processed raw =
      let tokens = extractTokens config raw
          selfExtractor name =
            case (Map.lookup name raws) of
              Just v ->
                if (Set.member name processed)
                  then Left $ CycleDeclaration $ T.unpack name
                  else valuesMapper (Set.insert name processed) v
              Nothing -> Left $ UnkonwnName $ T.unpack name
       in traverse (tokensMapper selfExtractor) tokens
    (errors, succeses) =
      (Map.mapEither (\item -> valuesMapper Set.empty item) raws)

tokensMapper ::
     (ValueName -> Either ValueParsingError [TemplateValueSegment])
  -> Token
  -> Either ValueParsingError TemplateValueSegment
tokensMapper baseValues token =
  case token of
    Const txt -> Right $ Single txt
    Value modifiers name ->
      case (baseValues name) of
        Left e         -> Left e
        Right segments -> Right $ Compound modifiers $ fromList segments
        
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

-- todo: add error for parseModifiers?
parseModifiers :: T.Text -> T.Text -> [Modifier]
parseModifiers separator modifiersBlock =
  let dropFirstSeparator block = T.drop (T.length separator) block
      modifiersCodes = T.splitOn separator (dropFirstSeparator modifiersBlock)
      maybeModifiers = map tryParseModifier modifiersCodes
      onlyParsed = filter (\item -> item /= Nothing) maybeModifiers
   in map (\(Just modifier) -> modifier) onlyParsed
