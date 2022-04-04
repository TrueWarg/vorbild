module Vorbild.Modifieble.Exec
  ( execModifications
  , SegmentParsingError(..)
  ) where

import           Control.Monad.Reader            (Reader, runReader)
import qualified Data.Bifunctor                  as Bif (first)
import qualified Data.Text                       as T
import           Vorbild.Either                  (eitherZipWith,
                                                  rightAccumulateWithList)
import qualified Vorbild.Modifieble.Block        as Block
import           Vorbild.TemplateValue.Placement

data SegmentParsingError =
  SegmentParsingError (Maybe String) String
  deriving (Show, Eq)

execModifications ::
     ValuesAndConfig
  -> T.Text
  -> [Block.Descriptor]
  -> Either SegmentParsingError T.Text
execModifications valuesAndConfig text descriptorConfigs =
  let placeResult =
        runReader (placeValuesToBlockList descriptorConfigs) valuesAndConfig
      modifyResult =
        fmap (\descriptors -> Block.modify text (descriptors)) placeResult
   in modifyResult

placeValuesToBlockList ::
     [Block.Descriptor]
  -> Reader ValuesAndConfig (Either SegmentParsingError [Block.Descriptor])
placeValuesToBlockList = (fmap foldr') . traverse placeValuesToBlock
  where
    foldr' = foldr rightAccumulateWithList (Right [])

placeValuesToBlock ::
     Block.Descriptor
  -> Reader ValuesAndConfig (Either SegmentParsingError Block.Descriptor)
placeValuesToBlock (Block.Descriptor label edges actions) = do
  edgesResult <- placeValuesToEdges label edges
  actionsResult <- fmap (mapActionError label) (placeValuesToActions actions)
  let result =
        eitherZipWith
          (\edges actions -> Block.Descriptor label edges actions)
          edgesResult
          actionsResult
  pure result

placeValuesToActions ::
     [Block.Action] -> Reader ValuesAndConfig (Either T.Text [Block.Action])
placeValuesToActions = (fmap foldr') . traverse placeValuesToAction
  where
    foldr' = foldr rightAccumulateWithList (Right [])

placeValuesToAction ::
     Block.Action -> Reader ValuesAndConfig (Either T.Text Block.Action)
placeValuesToAction action =
  case action of
    Block.Append arg ->
      placeTemplateValues arg >>= (\res -> pure $ fmap (Block.Append) res)
    Block.Prepend arg ->
      placeTemplateValues arg >>= (\res -> pure $ fmap (Block.Prepend) res)
    Block.AppendOnce arg ->
      placeTemplateValues arg >>= (\res -> pure $ fmap (Block.AppendOnce) res)
    Block.PrependOnce arg ->
      placeTemplateValues arg >>= (\res -> pure $ fmap (Block.PrependOnce) res)
    Block.SortLines -> pure $ Right Block.SortLines
    Block.SortLinesDesc -> pure $ Right Block.SortLinesDesc

mapActionError ::
     Maybe String
  -> (Either T.Text [Block.Action])
  -> (Either SegmentParsingError [Block.Action])
mapActionError label actions = Bif.first (mapper label) actions
  where
    mapper label value = SegmentParsingError label (T.unpack value)

placeValuesToEdges ::
     Maybe String
  -> Maybe Block.Edges
  -> Reader ValuesAndConfig (Either SegmentParsingError (Maybe Block.Edges))
placeValuesToEdges label mtext =
  case mtext of
    Nothing -> pure $ Right Nothing
    Just (Block.Edges start end) -> do
      let wrapInParsingError =
            Bif.first (\err -> SegmentParsingError label (T.unpack err))
      startResult <- fmap (wrapInParsingError) (placeTemplateValues start)
      endResult <- fmap (wrapInParsingError) (placeTemplateValues end)
      let result =
            eitherZipWith
              (\start end -> Just (Block.Edges start end))
              startResult
              endResult
      pure $ result
