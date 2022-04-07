module Vorbild.Modifieble.ExecIO
  ( execModificationsIO
  , ModificationError(..)
  ) where

import           Control.Monad.Reader            (runReader)
import qualified Data.Bifunctor                  as Bif (first)
import qualified Data.Map.Strict                 as Map
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Vorbild.Either                  (accumulate)
import qualified Vorbild.Modifieble.Config       as Config
import qualified Vorbild.Modifieble.Exec         as PureExec
import qualified Vorbild.Modifieble.Mapper       as Mapper
import           Vorbild.TemplateValue.Config    (PlaceholderConfig (..))
import           Vorbild.TemplateValue.Placement
import           Vorbild.TemplateValue.Segment

import           System.Directory                (doesFileExist)

data ModificationError
  = FileNotFound FilePath
  | ContentSegmentParsingError (Maybe String) String
  | ActionParsingError (Maybe String) String
  | PathSegmentParsingError FilePath String

execModificationsIO ::
     Map.Map TemplateValueId [TemplateValueSegment]
  -> PlaceholderConfig
  -> [Config.ModifiebleFile]
  -> IO (Either ModificationError ())
execModificationsIO values placeholderConfig = (fmap mapper) . exec
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
  let mapActionError (Mapper.ActionParsingError label action) =
        ActionParsingError label action
      mapSegmentError (PureExec.SegmentParsingError label value) =
        ContentSegmentParsingError label value
      modify =
        (Bif.first mapSegmentError) . PureExec.execModifications config content
      mapResult =
        Bif.first mapActionError (Mapper.mapBlockConfigList descriptorConfigs)
      modifyResult = mapResult >>= modify
  case modifyResult of
    Left err -> pure $ Left err
    Right modified -> do
      TIO.writeFile path modified
      pure $ Right ()

placePathTemplate ::
     ValuesAndConfig -> FilePath -> Either ModificationError FilePath
placePathTemplate config path =
  let wrapInParsingError path =
        Bif.first (\err -> PathSegmentParsingError path (T.unpack err))
      placeResult =
        wrapInParsingError
          path
          (runReader (placeTemplateValues (T.pack path)) config)
   in fmap (\packed -> T.unpack packed) placeResult
