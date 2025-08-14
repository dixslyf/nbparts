module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
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
    doc <- Pandoc.readMarkdown def {readerExtensions = Pandoc.pandocExtensions} mdText
    Pandoc.writeIpynb def doc

  -- Read metadata.
  -- TODO: Don't fail if file is missing — just warn.
  let metadataPath = nbpartsDir </> "metadata.yaml"
  (metadata :: Nbparts.Metadata) <- ExceptT (left Nbparts.PackParseMetadataError <$> Yaml.decodeFileEither metadataPath)

  let ipynbBytes = Text.encodeUtf8 ipynbText
  case Aeson.eitherDecodeStrict ipynbBytes of
    Right (nb :: Ipynb.Notebook Ipynb.NbV4) -> ExceptT $ eitherToIO (Aeson.encodeFile outputPath <$> fillMetadata nb metadata)
    Left _ ->
      case Aeson.eitherDecodeStrict ipynbBytes of
        Right (nb :: Ipynb.Notebook Ipynb.NbV3) -> ExceptT $ eitherToIO (Aeson.encodeFile outputPath <$> fillMetadata nb metadata)
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

eitherToIO :: Either a (IO ()) -> IO (Either a ())
eitherToIO (Left e) = pure (Left e)
eitherToIO (Right m) = Right <$> m
