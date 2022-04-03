{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Modifieble.Block
  ( Descriptor(..)
  , DescriptorId(..)
  , Action(..)
  , Edges(..)
  , modify
  ) where

import           Data.List    (intersperse, sort, sortBy)
import qualified Data.Text    as T (Text, concat, lines)
import           Vorbild.Text (breakOnThree)

data Descriptor =
  Descriptor
    { dId      :: DescriptorId
    , dEdges   :: Maybe Edges
    , dActions :: [Action]
    }
  deriving (Show, Eq)

data Edges =
  Edges T.Text T.Text
  deriving (Show, Eq)

newtype DescriptorId =
  DescriptorId T.Text
  deriving (Show, Eq)

data Action
  = Append T.Text
  | Prepend T.Text
  | SortLines
  | SortLinesDesc
  deriving (Show, Eq)

modify :: T.Text -> [Descriptor] -> T.Text
modify text [] = text
modify text (descriptor:descriptors) =
  modifySingle (modify text descriptors) descriptor

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
     Nothing -> applyActionList text actions
     Just (Edges start end) -> breakOnThree' start end text

applyAction :: T.Text -> Action -> T.Text
applyAction block action =
  case action of
    (Append other) -> block <> other
    (Prepend other) -> other <> block
    SortLines -> (T.concat $ intersperse "\n" $ sort $ T.lines block)
    SortLinesDesc ->
      (T.concat $ intersperse "\n" $ sortBy (flip compare) $ T.lines block)

applyActionList :: T.Text -> [Action] -> T.Text
applyActionList block [] = block
applyActionList block (action:actions) =
  applyActionList (applyAction block action) actions
