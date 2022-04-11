module Vorbild.Modifieble.Exec
  ( execModifications
  , ExecError(..)
  ) where

import           Control.Monad.Reader            (Reader, runReader)
import qualified Data.Bifunctor                  as Bif (first)
import qualified Data.Text                       as T
import           Vorbild.Either                  (eitherZipWith,
                                                  rightAccumulateWithList)
import qualified Vorbild.Modifieble.Block        as Block
import           Vorbild.TemplateValue.Placement

data ExecError
  = SegmentParsingError (Maybe String) String
  | BlockSplitError String String
  deriving (Show, Eq)

execModifications ::
     ValuesAndConfig
  -> T.Text
  -> [Block.Descriptor]
  -> Either ExecError T.Text
execModifications valuesAndConfig text descriptorConfigs =
  let placeResult =
        runReader (placeValuesToBlockList descriptorConfigs) valuesAndConfig
      errorMapper (Block.BlockSplitError start end) =
        BlockSplitError (T.unpack start) (T.unpack end)
      modify = (Bif.first errorMapper) . (Block.modify text)
      modifyResult = placeResult >>= modify
   in modifyResult

placeValuesToBlockList ::
     [Block.Descriptor]
  -> Reader ValuesAndConfig (Either ExecError [Block.Descriptor])
placeValuesToBlockList = (fmap foldr') . traverse placeValuesToBlock
  where
    foldr' = foldr rightAccumulateWithList (Right [])

placeValuesToBlock ::
     Block.Descriptor
  -> Reader ValuesAndConfig (Either ExecError Block.Descriptor)
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
  -> (Either ExecError [Block.Action])
mapActionError label actions = Bif.first (mapper label) actions
  where
    mapper label value = SegmentParsingError label (T.unpack value)

placeValuesToEdges ::
     Maybe String
  -> Maybe Block.Edges
  -> Reader ValuesAndConfig (Either ExecError (Maybe Block.Edges))
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
