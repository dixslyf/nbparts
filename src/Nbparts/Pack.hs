module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (confIndent))
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Nbparts.Pack.Metadata qualified as Nbparts
import Nbparts.Pack.Outputs qualified as Nbparts
import Nbparts.Pack.Sources qualified as Nbparts
import Nbparts.Pack.Sources.Markdown qualified as Nbparts
import Nbparts.Types qualified as Nbparts
import System.FilePath ((<.>), (</>))
import System.FilePath qualified as FilePath

data PackOptions = PackOptions
  { directory :: FilePath,
    outputPath :: Maybe FilePath
  }

pack :: (MonadError Nbparts.PackError m, MonadIO m) => PackOptions -> m ()
pack (PackOptions nbpartsDir maybeOutputPath) = do
  -- `nbpartsDir` should be in the form "some_notebook.ipynb.nbparts".
  let fallbackOutputPath = FilePath.dropExtension nbpartsDir
  let outputPath = Maybe.fromMaybe fallbackOutputPath maybeOutputPath

  -- Read manifest, metadata, sources and outputs.
  let mkImportPath :: FilePath -> Nbparts.Format -> FilePath
      mkImportPath fname fmt = nbpartsDir </> fname <.> Nbparts.formatExtension fmt

  let manifestPath = mkImportPath "nbparts" Nbparts.FormatYaml
  ( Nbparts.Manifest
      { nbpartsVersion = _nbpartsVersion,
        sourcesFormat,
        metadataFormat,
        outputsFormat
      }
    ) <-
    liftEither =<< liftIO (left Nbparts.PackParseManifestError <$> Yaml.decodeFileEither manifestPath)

  -- TODO: Don't fail if metadata and outputs are missing — just warn.
  let sourcesPath = mkImportPath "sources" sourcesFormat
  (sources :: [Nbparts.CellSource]) <- case sourcesFormat of
    Nbparts.FormatYaml -> do
      res <- liftIO $ Yaml.decodeFileEither sourcesPath
      liftEither $ left Nbparts.PackParseYamlSourcesError res
    Nbparts.FormatJson -> do
      res <- liftIO $ Aeson.eitherDecodeFileStrict sourcesPath
      liftEither $ left (Nbparts.PackParseJsonSourcesError . Text.pack) res
    Nbparts.FormatMarkdown -> do
      mdText <- liftIO $ Text.readFile sourcesPath
      liftEither $ Nbparts.markdownToSources sourcesPath mdText

  let metadataPath = mkImportPath "metadata" metadataFormat
  (metadata :: Nbparts.NotebookMetadata) <- case metadataFormat of
    Nbparts.FormatYaml -> do
      res <- liftIO $ Yaml.decodeFileEither metadataPath
      liftEither $ left Nbparts.PackParseYamlMetadataError res
    Nbparts.FormatJson -> do
      res <- liftIO $ Aeson.eitherDecodeFileStrict metadataPath
      liftEither $ left (Nbparts.PackParseJsonMetadataError . Text.pack) res
    _ -> throwError $ Nbparts.PackIllegalFormatError Nbparts.IllegalFormatMetadata metadataFormat

  let outputsPath = mkImportPath "outputs" outputsFormat
  (unembeddedOutputs :: Nbparts.UnembeddedNotebookOutputs) <- case outputsFormat of
    Nbparts.FormatYaml -> do
      res <- liftIO $ Yaml.decodeFileEither outputsPath
      liftEither $ left Nbparts.PackParseYamlOutputsError res
    Nbparts.FormatJson -> do
      res <- liftIO $ Aeson.eitherDecodeFileStrict outputsPath
      liftEither $ left (Nbparts.PackParseJsonOutputsError . Text.pack) res
    _ -> throwError $ Nbparts.PackIllegalFormatError Nbparts.IllegalFormatOutputs outputsFormat

  let (Nbparts.NotebookMetadata major minor _ _) = metadata
  nb <- case major of
    4 -> pure $ Nbparts.SomeNotebook $ (emptyNotebook @Ipynb.NbV4) (major, minor)
    _ -> throwError $ Nbparts.PackUnsupportedNotebookFormat (major, minor)

  -- Create and export the notebook.
  filledNb <-
    Nbparts.withSomeNotebook
      nb
      ( liftIO . Nbparts.fillSources nbpartsDir sources
          >=> liftEither . Nbparts.fillMetadata metadata
          >=> Nbparts.fillOutputs nbpartsDir unembeddedOutputs
          >=> pure . Nbparts.SomeNotebook
      )

  liftIO $ exportJson outputPath filledNb

prettyConfig :: AesonPretty.Config
prettyConfig = AesonPretty.defConfig {confIndent = AesonPretty.Spaces 1}

exportJson :: (Aeson.ToJSON (a)) => FilePath -> a -> IO ()
exportJson fp = LazyByteString.writeFile fp . AesonPretty.encodePretty' prettyConfig

emptyNotebook :: (Int, Int) -> Ipynb.Notebook a
emptyNotebook format = Ipynb.Notebook (Ipynb.JSONMeta Map.empty) format []
