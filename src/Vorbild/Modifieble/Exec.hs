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
import           Vorbild.Either                  (accumulate,
                                                  rightAccumulateWithList,
                                                  eitherZipWith,
                                                  eitherZipWith4)
import qualified Vorbild.Modifieble.Block        as Block
import qualified Vorbild.Modifieble.Config       as Config
import           Vorbild.TemplateValue.Config    (PlaceholderConfig (..))
import           Vorbild.TemplateValue.Placement
import           Vorbild.TemplateValue.Segment

data ModificationError
  = FileNotFound String
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
           let path = Config.filePath config
               rootDescriptor = Config.rootDescriptor config
               valuesAndConfig = ValuesAndConfig values placeholderConfig
           isExist <- doesFileExist path
           if (not isExist)
             then pure (Left $ FileNotFound path)
             else execModificationIntenal valuesAndConfig path rootDescriptor)
    mapper = foldl accumulate (Right ())

execModificationIntenal ::
     ValuesAndConfig
  -> FilePath
  -> Config.BlockDescriptorItem
  -> IO (Either ModificationError ())
execModificationIntenal config path descriptorConfig = do
  content <- TIO.readFile path
  let mapResult = runReader (mapConfigToBlock descriptorConfig) config
      modifyResult =
        fmap (\descriptor -> Block.modify content (descriptor : [])) mapResult
  case modifyResult of
    Left err -> pure $ Left err
    Right modified -> do
      TIO.writeFile path modified
      pure $ Right ()

mapConfigToBlock ::
     Config.BlockDescriptorItem
  -> Reader ValuesAndConfig (Either ModificationError Block.Descriptor)
mapConfigToBlock (Config.BlockDescriptorItem id start end actions сhildren) = do
  let wrapInParsingError configId =
        Bif.first
          (\err -> SegmentParsingError (T.unpack configId) (T.unpack err))
  startResult <- fmap (wrapInParsingError id) (placeTemplateValues start)
  endResult <- fmap (wrapInParsingError id) (placeTemplateValues end)
  actionsResult <- fmap (mapActionError id) (mapActions actions)
  childrenResult <- mapConfigToBlockList сhildren
  let result =
        eitherZipWith4
          (\start end actions children ->
             Block.Descriptor (Block.DescriptorId id) start end actions children)
          startResult
          endResult
          actionsResult
          childrenResult
  pure result

mapConfigToBlockList ::
     [Config.BlockDescriptorItem]
  -> Reader ValuesAndConfig (Either ModificationError [Block.Descriptor])
mapConfigToBlockList [] = pure $ Right []
mapConfigToBlockList (config:configs) = do
  configMapResult <- mapConfigToBlock config
  remainingMapResult <- mapConfigToBlockList configs
  pure $
    eitherZipWith
      (\descriptor other -> descriptor : other)
      configMapResult
      remainingMapResult

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
