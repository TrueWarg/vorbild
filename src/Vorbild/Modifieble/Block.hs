{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Modifieble.Block where

import           Data.List(sort, sortBy)
import qualified Data.Text    as T
import           Vorbild.Text (breakOnThree)

data Descriptor =
  Descriptor
    { dStart    :: T.Text
    , dEnd      :: T.Text
    , dActions  :: [Action]
    , dChildren :: [Descriptor]
    }

data Action
  = Append T.Text
  | Prepend T.Text
  | SortLines
  | SortLinesDesc

modify :: T.Text -> [Descriptor] -> T.Text
modify text [] = text
modify text (descriptor:descriptors) =
  modifySingle (modify text (dChildren descriptor <> descriptors)) descriptor

modifySingle :: T.Text -> Descriptor -> T.Text
modifySingle text descriptor =
  let start = dStart descriptor
      end = dEnd descriptor
      actions = dActions descriptor
      result = breakOnThree start end text
   in case result of
        (Left err) -> text
        Right (beforeStartIncl, body, afterEndIncl) ->
          beforeStartIncl <> applyActionList body actions <> afterEndIncl

applyAction :: T.Text -> Action -> T.Text
applyAction block action =
  case action of
    (Append other)  -> block <> other
    (Prepend other) -> other <> block
    SortLines       -> T.concat $ sort $ T.lines block
    SortLinesDesc   -> T.concat $ sortBy (flip compare) $ T.lines block

applyActionList :: T.Text -> [Action] -> T.Text
applyActionList block [] = block
applyActionList block (action:actions) =
  applyActionList (applyAction block action) actions
