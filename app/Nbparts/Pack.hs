module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Ipynb (Output (..))
import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Nbparts.Pack.Error (PackError)
import Nbparts.Pack.Error qualified as Nbparts
import Nbparts.Unpack.Metadata qualified as Nbparts
import Nbparts.Unpack.Outputs qualified as Nbparts
import System.FilePath ((<.>), (</>))
import System.FilePath qualified as FilePath
import Text.Pandoc (ReaderOptions (..))
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Options (def)

pack :: FilePath -> Maybe FilePath -> IO (Either PackError ())
pack nbpartsDir maybeOutputPath = runExceptT $ do
  -- `nbpartsDir` should be in the form "some_notebook.ipynb.nbparts".
  let fallbackOutputPath = FilePath.dropExtension nbpartsDir
  let basename = FilePath.dropExtension $ FilePath.takeBaseName fallbackOutputPath
  let outputPath = Maybe.fromMaybe fallbackOutputPath maybeOutputPath

  -- Convert the markdown into ipynb.
  let mdPath = nbpartsDir </> basename <.> "md"
  mdText <- liftIO $ Text.readFile mdPath
  ipynbText <- ExceptT $ fmap (left Nbparts.PackPandocError) $ Pandoc.runIO $ do
    Pandoc.setResourcePath [nbpartsDir]
    doc <- Pandoc.readMarkdown def {readerExtensions = Pandoc.pandocExtensions} mdText
    Pandoc.writeIpynb def doc

  -- Read metadata.
  -- TODO: Don't fail if file is missing — just warn.
  let metadataPath = nbpartsDir </> "metadata.yaml"
  (metadata :: Nbparts.Metadata) <- ExceptT (left Nbparts.PackParseMetadataError <$> Yaml.decodeFileEither metadataPath)

  -- Read outputs.
  let outputsPath = nbpartsDir </> "outputs.yaml"
  (unembeddedOutputs :: Nbparts.UnembeddedOutputs) <- ExceptT (left Nbparts.PackParseOutputsError <$> Yaml.decodeFileEither outputsPath)

  let processNb :: Ipynb.Notebook a -> (FilePath -> Ipynb.Notebook a -> IO ()) -> ExceptT PackError IO ()
      processNb nb encode = do
        nbWithMeta <- ExceptT $ pure $ fillMetadata nb metadata
        nbWithMetaAndOutputs <- ExceptT $ fillOutputs nbpartsDir unembeddedOutputs nbWithMeta
        liftIO $ encode outputPath nbWithMetaAndOutputs

  let ipynbBytes = Text.encodeUtf8 ipynbText
  case Aeson.eitherDecodeStrict ipynbBytes of
    Right (nb :: Ipynb.Notebook Ipynb.NbV4) -> processNb nb Aeson.encodeFile
    Left _ ->
      case Aeson.eitherDecodeStrict ipynbBytes of
        Right (nb :: Ipynb.Notebook Ipynb.NbV3) -> processNb nb Aeson.encodeFile
        Left message -> ExceptT $ pure $ Left $ Nbparts.PackParseIpynbError (Text.pack message)

fillMetadata :: Ipynb.Notebook a -> Nbparts.Metadata -> Either Nbparts.PackError (Ipynb.Notebook a)
fillMetadata (Ipynb.Notebook _ format cells) (Nbparts.Metadata nbMeta cellsMeta) = Ipynb.Notebook nbMeta format <$> filledCells
  where
    filledCells = traverse (fillCellMetadata cellsMeta) cells

fillCellMetadata :: Map Text Ipynb.JSONMeta -> Ipynb.Cell a -> Either Nbparts.PackError (Ipynb.Cell a)
fillCellMetadata cellsMeta (Ipynb.Cell cellType maybeCellId source _ attachments) = do
  cellId <- maybe (Left Nbparts.PackMissingCellIdError) Right maybeCellId
  cellMeta <- maybe (Left $ Nbparts.PackMissingCellMetadataError cellId) Right (Map.lookup cellId cellsMeta)
  pure $ Ipynb.Cell cellType (Just cellId) source cellMeta attachments

fillOutputs :: FilePath -> Nbparts.UnembeddedOutputs -> Ipynb.Notebook a -> IO (Either Nbparts.PackError (Ipynb.Notebook a))
fillOutputs prefixDir unembeddedOutputs (Ipynb.Notebook meta format cells) = runExceptT $ do
  outputs <- liftIO $ (embedOutputs . adjustOutputsPaths prefixDir) unembeddedOutputs
  filledCells <- (ExceptT . pure) $ traverse (fillCellOutputs outputs) cells
  return $ Ipynb.Notebook meta format filledCells

fillCellOutputs :: Nbparts.Outputs a -> Ipynb.Cell a -> Either Nbparts.PackError (Ipynb.Cell a)
fillCellOutputs outputs (Ipynb.Cell (Ipynb.Code codeExecutionCount _) maybeCellId source meta attachments) = do
  cellId <- maybe (Left Nbparts.PackMissingCellIdError) Right maybeCellId
  cellOutputs <- maybe (Left $ Nbparts.PackMissingCellOutputsError cellId) Right (Map.lookup cellId outputs)
  return $ Ipynb.Cell (Ipynb.Code codeExecutionCount cellOutputs) (Just cellId) source meta attachments
fillCellOutputs _ cell = pure cell

adjustOutputsPaths :: FilePath -> Nbparts.UnembeddedOutputs -> Nbparts.UnembeddedOutputs
adjustOutputsPaths prefixDir = Map.map (map $ adjustOutputPaths prefixDir)

adjustOutputPaths :: FilePath -> Nbparts.UnembeddedOutput -> Nbparts.UnembeddedOutput
adjustOutputPaths prefixDir (Nbparts.DisplayData displayData metadata) =
  Nbparts.DisplayData (adjustMimeBundlePaths prefixDir displayData) metadata
adjustOutputPaths prefixDir (Nbparts.ExecuteResult count executeData metadata) =
  Nbparts.ExecuteResult count (adjustMimeBundlePaths prefixDir executeData) metadata
adjustOutputPaths _ output = output

adjustMimeBundlePaths :: FilePath -> Nbparts.UnembeddedMimeBundle -> Nbparts.UnembeddedMimeBundle
adjustMimeBundlePaths prefixDir = Map.map (adjustMimeDataPaths prefixDir)

adjustMimeDataPaths :: FilePath -> Nbparts.UnembeddedMimeData -> Nbparts.UnembeddedMimeData
adjustMimeDataPaths prefixDir (Nbparts.BinaryData path) = Nbparts.BinaryData $ prefixDir </> path
adjustMimeDataPaths _ mimeData = mimeData

embedOutputs :: Nbparts.UnembeddedOutputs -> IO (Nbparts.Outputs a)
embedOutputs = traverse $ mapM embedOutput

embedOutput :: Nbparts.UnembeddedOutput -> IO (Ipynb.Output a)
embedOutput (Nbparts.Stream streamName streamText) = pure $ Ipynb.Stream {streamName, streamText = Ipynb.Source streamText}
embedOutput (Nbparts.DisplayData unembeddedDisplayData displayMetadata) = do
  displayData <- embedMimeBundle unembeddedDisplayData
  return
    Ipynb.DisplayData
      { displayData = displayData,
        displayMetadata
      }
embedOutput (Nbparts.ExecuteResult executeCount unembeddedExecuteData executeMetadata) = do
  executeData <- embedMimeBundle unembeddedExecuteData
  return
    Ipynb.ExecuteResult
      { executeCount,
        executeData = executeData,
        executeMetadata
      }
embedOutput (Nbparts.Err errName errValue errTraceback) = pure $ Ipynb.Err {errName, errValue, errTraceback}

embedMimeBundle :: Nbparts.UnembeddedMimeBundle -> IO Ipynb.MimeBundle
embedMimeBundle unembeddedMimeBundle = Ipynb.MimeBundle <$> traverse embedMimeData unembeddedMimeBundle

embedMimeData :: Nbparts.UnembeddedMimeData -> IO Ipynb.MimeData
embedMimeData (Nbparts.BinaryData path) = do
  bytes <- ByteString.readFile path
  return $ Ipynb.BinaryData bytes
embedMimeData (Nbparts.TextualData text) = pure $ Ipynb.TextualData text
embedMimeData (Nbparts.JsonData value) = pure $ Ipynb.JsonData value

eitherToIO :: Either a (IO ()) -> IO (Either a ())
eitherToIO (Left e) = pure (Left e)
eitherToIO (Right m) = Right <$> m
