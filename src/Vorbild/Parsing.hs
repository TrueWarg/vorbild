{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Parsing
  ( PlaceholderSeparator
  , PlaceholderPrefix
  , parseValues
  )
  where

import Vorbild.TemplateValue
import Vorbild.ValueConfig
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List
import Data.List.NonEmpty(fromList)

newtype PlaceholderSeparator = PlaceholderSeparator T.Text
newtype PlaceholderPrefix = PlaceholderPrefix T.Text

defaultSeparator = PlaceholderSeparator "||"
defaultPrefix = PlaceholderPrefix "::"

data Token 
    = Const T.Text
    | Value T.Text

parseValues :: Map.Map ValueName RawValue -> Map.Map TemplateValueId [TemplateValueSegment]
parseValues raws = if (Map.null raws) then Map.empty
                   else Map.mapKeys TemplateValueId (Map.map valuesMapper raws)   
            where
                valuesMapper raw = let
                    tokens = extractTokens raw
                    tokensMapper token = 
                        case token of
                            Const txt -> Single txt
                            Value name -> Compound $ fromList (valuesMapper (raws Map.! name))
                    in
                        map tokensMapper tokens   

-- todo: use Reader to extract some Token Value detector and extractor
-- curently const||::ValueName:modifier||const
extractTokens :: T.Text -> [Token]
extractTokens "" = [Const ""]
extractTokens line =
    let
        (PlaceholderSeparator separator) = defaultSeparator
        (PlaceholderPrefix prefix) = defaultPrefix
        splitted = filter (\txt -> not $ T.null txt) (T.splitOn separator line)
        transform = 
            (\txt -> 
              if (T.isPrefixOf prefix txt) 
                  then Value $ T.drop (T.length prefix) txt
                  else Const txt
            )
    in
        map transform splitted