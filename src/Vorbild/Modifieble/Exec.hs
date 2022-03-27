{-# LANGUAGE OverloadedStrings #-}

module Vorbild.Modifieble.Exec
  ( execModifications
  , ModificationError(..)
  ) where

import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Vorbild.Either            (accumulate)
import qualified Vorbild.Modifieble.Block  as Block
import qualified Vorbild.Modifieble.Config as Config

import           System.Directory          (doesFileExist)

data ModificationError
  = FileNotFound String
  | ExecFail String String
  deriving (Show, Eq)

execModifications :: [Config.ModifiebleFile] -> IO (Either ModificationError ())
execModifications = (fmap mapper) . exec
  where
    exec =
      traverse
        (\config -> do
           let path = Config.mfPath config
               rootDescriptor = Config.rootDescriptor config
           isExist <- doesFileExist path
           if (not isExist)
             then pure (Left $ FileNotFound path)
             else execModificationIntenal path rootDescriptor)
    mapper = foldl accumulate (Right ())

execModificationIntenal ::
     FilePath -> Config.BlockDescriptorItem -> IO (Either ModificationError ())
execModificationIntenal path descriptorConfig = do
  content <- TIO.readFile path
  let descriptor = mapConfigToBlock descriptorConfig
      modified = Block.modify content (descriptor : [])
  TIO.writeFile path modified
  pure $ Right ()  

mapConfigToBlock :: Config.BlockDescriptorItem -> Block.Descriptor
mapConfigToBlock (Config.BlockDescriptorItem id start end actions сhildren) =
  Block.Descriptor
    (Block.DescriptorId id)
    start
    end
    (mapActions actions)
    (map mapConfigToBlock сhildren)

mapActions :: [T.Text] -> [Block.Action]
mapActions [] = []
mapActions (code : codes) = (mapper $ actionNameAndArg code) : mapActions codes
  where
      mapper (name, arg)
        | name == "append" = Block.Append arg
        | name == "prepend" = Block.Prepend arg
        | name == "sortLines" = Block.SortLines
        | name == "sortLinesDesc" = Block.SortLinesDesc
        -- todo add Either error
        | otherwise = error $ "unkown actinon name " <> T.unpack name

actionNameAndArg :: T.Text -> (T.Text, T.Text)
actionNameAndArg action = 
    let 
        (name, arg) = T.break (==':') action
        clearName = T.strip name
        clearArg = parseReplaceArg $ T.strip $ T.drop 1 arg
    in
        (clearName, clearArg)

parseReplaceArg :: T.Text -> T.Text
parseReplaceArg arg
  | T.head arg == '\'' && T.last arg == '\'' =
    T.dropAround (\ch -> ch == '\'') arg
  | otherwise = arg
  