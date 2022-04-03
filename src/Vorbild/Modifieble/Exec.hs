{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Modifieble.Exec
  ( execModifications
  , ModificationError(..)
  ) where

import           Control.Monad.Reader            (Reader, runReader)
import qualified Data.Bifunctor                  as Bif (first)
import qualified Data.Map.Strict                 as Map
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           System.Directory                (doesFileExist)
import           Vorbild.Either                  (accumulate, eitherZipWith,
                                                  rightAccumulateWithList)
import qualified Vorbild.Modifieble.Block        as Block
import qualified Vorbild.Modifieble.Config       as Config
import           Vorbild.TemplateValue.Config    (PlaceholderConfig (..))
import           Vorbild.TemplateValue.Placement
import           Vorbild.TemplateValue.Segment

data ModificationError
  = FileNotFound FilePath
  | FileSegmentParsingError FilePath String
  | ExecFail String String
  | SegmentParsingError String String
  | ActionParsingError String String
  deriving (Show, Eq)

data ActionError
  = ArgParsingError String
  | NameParsingError String
  deriving (Show, Eq)

execModifications ::
     Map.Map TemplateValueId [TemplateValueSegment]
  -> PlaceholderConfig
  -> [Config.ModifiebleFile]
  -> IO (Either ModificationError ())
execModifications values placeholderConfig = (fmap mapper) . exec
  where
    exec =
      traverse
        (\config -> do
           let valuesAndConfig = ValuesAndConfig values placeholderConfig
               pathResult =
                 placePathTemplate valuesAndConfig (Config.filePath config)
               descriptors = Config.blockDescriptors config
           case pathResult of
             Left e -> pure $ Left e
             Right path -> do
               isExist <- doesFileExist path
               if (not isExist)
                 then pure (Left $ FileNotFound path)
                 else execModificationIntenal valuesAndConfig path descriptors)
    mapper = foldr accumulate (Right ())

execModificationIntenal ::
     ValuesAndConfig
  -> FilePath
  -> [Config.BlockDescriptorItem]
  -> IO (Either ModificationError ())
execModificationIntenal config path descriptorConfigs = do
  content <- TIO.readFile path
  let mapResult = runReader (mapConfigToBlockList descriptorConfigs) config
      modifyResult =
        fmap (\descriptors -> Block.modify content (descriptors)) mapResult
  case modifyResult of
    Left err -> pure $ Left err
    Right modified -> do
      TIO.writeFile path modified
      pure $ Right ()

placePathTemplate ::
     ValuesAndConfig -> FilePath -> Either ModificationError FilePath
placePathTemplate config path =
  let wrapInParsingError path =
        Bif.first (\err -> FileSegmentParsingError path (T.unpack err))
      placeResult =
        wrapInParsingError
          path
          (runReader (placeTemplateValues (T.pack path)) config)
   in fmap (\packed -> T.unpack packed) placeResult

mapConfigToBlockList ::
     [Config.BlockDescriptorItem]
  -> Reader ValuesAndConfig (Either ModificationError [Block.Descriptor])
mapConfigToBlockList = (fmap foldr') . traverse mapConfigToBlock
  where
    foldr' = foldr rightAccumulateWithList (Right [])

mapConfigToBlock ::
     Config.BlockDescriptorItem
  -> Reader ValuesAndConfig (Either ModificationError Block.Descriptor)
mapConfigToBlock (Config.BlockDescriptorItem id edges actions) = do
  edgesResult <- mapEdges id edges
  actionsResult <- fmap (mapActionError id) (mapActions actions)
  let result =
        eitherZipWith
          (\edges actions ->
             Block.Descriptor (Block.DescriptorId id) edges actions)
          edgesResult
          actionsResult
  pure result

mapActions ::
     [T.Text] -> Reader ValuesAndConfig (Either ActionError [Block.Action])
mapActions = (fmap foldr') . traverse mapAction
  where
    foldr' = foldr rightAccumulateWithList (Right [])

mapAction :: T.Text -> Reader ValuesAndConfig (Either ActionError Block.Action)
mapAction code = do
  let (name, arg) = actionNameAndArg code
      mapper name arg
        | name == "append" = Right $ Block.Append arg
        | name == "prepend" = Right $ Block.Prepend arg
        | name == "sortLines" = Right Block.SortLines
        | name == "sortLinesDesc" = Right Block.SortLinesDesc
        | otherwise = Left $ NameParsingError $ T.unpack name
      errorMapper = Bif.first (\err -> ArgParsingError $ T.unpack err)
  argResult <- fmap errorMapper (placeTemplateValues arg)
  pure $ (argResult >>= (mapper name))

mapActionError ::
     T.Text
  -> (Either ActionError [Block.Action])
  -> (Either ModificationError [Block.Action])
mapActionError configId actions = Bif.first (mapper $ T.unpack configId) actions
  where
    mapper configId error =
      case error of
        (ArgParsingError value)  -> SegmentParsingError configId value
        (NameParsingError value) -> ActionParsingError configId value

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

mapEdges ::
     T.Text
  -> Maybe Config.BlockEdges
  -> Reader ValuesAndConfig (Either ModificationError (Maybe Block.Edges))
mapEdges configId mtext =
  case mtext of
    Nothing -> pure $ Right Nothing
    Just (Config.BlockEdges start end) -> do
      let wrapInParsingError =
            Bif.first
              (\err -> SegmentParsingError (T.unpack configId) (T.unpack err))
      startResult <- fmap (wrapInParsingError) (placeTemplateValues start)
      endResult <- fmap (wrapInParsingError) (placeTemplateValues end)
      let result =
            eitherZipWith
              (\start end -> Just (Block.Edges start end))
              startResult
              endResult
      pure $ result
