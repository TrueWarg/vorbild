{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Modifieble.Block
  ( Descriptor(..)
  , Action(..)
  , Edges(..)
  , modify
  ) where

import           Data.List    (intersperse, sort, sortBy)
import qualified Data.Text    as T (Text, concat, lines)
import           Vorbild.Text (breakOnThree, isSubText)

data Descriptor =
  Descriptor
    { dLabel   :: Maybe String
    , dEdges   :: Maybe Edges
    , dActions :: [Action]
    }
  deriving (Show, Eq)

data Edges =
  Edges T.Text T.Text
  deriving (Show, Eq)

data Action
  = Append T.Text
  | Prepend T.Text
  | AppendOnce T.Text
  | PrependOnce T.Text
  | SortLines
  | SortLinesDesc
  deriving (Show, Eq)

modify :: T.Text -> [Descriptor] -> T.Text
modify text [] = text
modify text (descriptor:descriptors) =
  modify (modifySingle text descriptor) descriptors

modifySingle :: T.Text -> Descriptor -> T.Text
modifySingle text descriptor =
  let edges = dEdges descriptor
      actions = dActions descriptor
      breakOnThree' start end txt =
        case (breakOnThree start end txt) of
          (Left _) -> text
          Right (beforeStartIncl, body, afterEndIncl) ->
            beforeStartIncl <> applyActionList body actions <> afterEndIncl
   in case edges of
        Nothing                -> applyActionList text actions
        Just (Edges start end) -> breakOnThree' start end text

applyAction :: T.Text -> Action -> T.Text
applyAction block action =
  case action of
    (Append other) -> block <> other
    (Prepend other) -> other <> block
    (AppendOnce other) ->
      if (isSubText other block)
        then block
        else block <> other
    (PrependOnce other) ->
      if (isSubText other block)
        then block
        else other <> block
    SortLines -> (T.concat $ intersperse "\n" $ sort $ T.lines block)
    SortLinesDesc ->
      (T.concat $ intersperse "\n" $ sortBy (flip compare) $ T.lines block)

applyActionList :: T.Text -> [Action] -> T.Text
applyActionList block [] = block
applyActionList block (action:actions) =
  applyActionList (applyAction block action) actions
