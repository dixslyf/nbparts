module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (..))
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Nbparts.Pack.Error (PackError)
import Nbparts.Pack.Error qualified as Nbparts
import Nbparts.Pack.Metadata qualified as Nbparts
import Nbparts.Pack.Outputs qualified as Nbparts
import Nbparts.Pack.Sources qualified as Nbparts
import Nbparts.Pack.Sources.Markdown qualified as Nbparts
import Nbparts.Types (NbpartsManifest (NbpartsManifest))
import Nbparts.Types qualified as Nbparts
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

data PackOptions = PackOptions
  { directory :: FilePath,
    outputPath :: Maybe FilePath
  }

pack :: (MonadError PackError m, MonadIO m) => PackOptions -> m ()
pack (PackOptions nbpartsDir maybeOutputPath) = do
  -- `nbpartsDir` should be in the form "some_notebook.ipynb.nbparts".
  let fallbackOutputPath = FilePath.dropExtension nbpartsDir
  let outputPath = Maybe.fromMaybe fallbackOutputPath maybeOutputPath

  -- Read manifest, metadata, sources and outputs.
  let manifestPath = nbpartsDir </> "nbparts.yaml"
  (NbpartsManifest _nbpartsVersion sourcesFormat) <- liftEither =<< liftIO (left Nbparts.PackParseManifestError <$> Yaml.decodeFileEither manifestPath)

  -- TODO: Don't fail if metadata and outputs are missing — just warn.
  (sources :: [Nbparts.Source]) <- case sourcesFormat of
    Nbparts.FormatYaml -> do
      let sourcesPath = nbpartsDir </> "sources.yaml"
      liftEither =<< liftIO (left Nbparts.PackParseYamlSourcesError <$> Yaml.decodeFileEither sourcesPath)
    Nbparts.FormatMarkdown -> do
      let sourcesPath = nbpartsDir </> "sources.md"
      mdText <- liftIO $ Text.readFile sourcesPath
      liftEither $ Nbparts.markdownToSources sourcesPath mdText

  let metadataPath = nbpartsDir </> "metadata.yaml"
  let outputsPath = nbpartsDir </> "outputs.yaml"
  (metadata :: Nbparts.Metadata) <- liftEither =<< liftIO (left Nbparts.PackParseMetadataError <$> Yaml.decodeFileEither metadataPath)
  (unembeddedOutputs :: Nbparts.UnembeddedOutputs) <- liftEither =<< liftIO (left Nbparts.PackParseOutputsError <$> Yaml.decodeFileEither outputsPath)

  let (Nbparts.Metadata major minor _ _) = metadata
  nb <- case major of
    3 -> pure $ Nbparts.SomeNotebook $ (emptyNotebook @Ipynb.NbV3) (major, minor)
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
