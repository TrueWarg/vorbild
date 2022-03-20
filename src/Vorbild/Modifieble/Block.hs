{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Modifieble.Block where

import qualified Data.Text as T

data Descriptor =
  Descriptor
    { start    :: Maybe T.Text
    , end      :: Maybe T.Text
    , body     :: T.Text
    , actions  :: [Action]
    , children :: [Descriptor]
    }

data Action
  = Append T.Text
  | Prepend T.Text
  | SortLines
  | SortLinesDesc

applyAction :: T.Text -> Action -> T.Text
applyAction block action =
  case action of
    (Append other)  -> block <> other
    (Prepend other) -> other <> block
    SortLines       -> sort $ T.lines block
    SortLinesDesc   -> sortBy (flip compare) $ T.lines block

applyActionList :: T.Text -> [Action] -> T.Text
applyActionList block [] = block
applyActionList block (action:actions) =
  applyActionList (applyAction block action) actions
