{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Modifieble.Mapper
  ( mapBlockConfigList
  , ActionParsingError(..)
  ) where

import qualified Data.Text                 as T
import           Vorbild.Either            (rightAccumulateWithList)
import qualified Vorbild.Modifieble.Block  as Block
import qualified Vorbild.Modifieble.Config as Config

data ActionParsingError =
  ActionParsingError (Maybe String) String

mapBlockConfigList ::
     [Config.BlockDescriptorItem]
  -> Either ActionParsingError [Block.Descriptor]
mapBlockConfigList = foldr' . map mapBlockConfig
  where
    foldr' = foldr rightAccumulateWithList (Right [])

mapBlockConfig ::
     Config.BlockDescriptorItem -> Either ActionParsingError Block.Descriptor
mapBlockConfig (Config.BlockDescriptorItem label edges actionCodes) =
  let actionsMapResult = mapActionList label actionCodes
   in case actionsMapResult of
        Left e -> Left e
        Right actions ->
          Right $ Block.Descriptor label (fmap mapEdges edges) actions

mapEdges :: Config.BlockEdges -> Block.Edges
mapEdges (Config.BlockEdges start end) = Block.Edges start end

mapActionList ::
     Maybe String -> [T.Text] -> Either ActionParsingError [Block.Action]
mapActionList label = foldr' . map (mapAction label)
  where
    foldr' = foldr rightAccumulateWithList (Right [])

mapAction :: Maybe String -> T.Text -> Either ActionParsingError Block.Action
mapAction label code =
  let (name, arg) = actionNameAndArg code
      mapper (name, arg)
        | name == "append" = Right $ Block.Append arg
        | name == "prepend" = Right $ Block.Prepend arg
        | name == "appendOnce" = Right $ Block.AppendOnce arg
        | name == "prependOnce" = Right $ Block.PrependOnce arg
        | name == "sortLines" = Right Block.SortLines
        | name == "sortLinesDesc" = Right Block.SortLinesDesc
        | otherwise = Left $ ActionParsingError label (T.unpack name)
   in mapper $ actionNameAndArg code

actionNameAndArg :: T.Text -> (T.Text, T.Text)
actionNameAndArg action =
  let (name, arg) = T.break (== ':') action
      clearName = T.strip name
      clearArg = parseReplaceArg $ T.strip $ T.drop 1 arg
   in (clearName, clearArg)

parseReplaceArg :: T.Text -> T.Text
parseReplaceArg arg
  | T.null arg = arg
  | T.head arg == '\'' && T.last arg == '\'' =
    T.dropAround (\ch -> ch == '\'') arg
  | otherwise = arg
