module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (..))
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Yaml qualified as Yaml
import Nbparts.Pack.Error (PackError)
import Nbparts.Pack.Error qualified as Nbparts
import Nbparts.Pack.Metadata qualified as Nbparts
import Nbparts.Pack.Outputs qualified as Nbparts
import Nbparts.Pack.Sources qualified as Nbparts
import Nbparts.Types qualified as Nbparts
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

pack :: (MonadError PackError m, MonadIO m) => FilePath -> Maybe FilePath -> m ()
pack nbpartsDir maybeOutputPath = do
  -- `nbpartsDir` should be in the form "some_notebook.ipynb.nbparts".
  let fallbackOutputPath = FilePath.dropExtension nbpartsDir
  let outputPath = Maybe.fromMaybe fallbackOutputPath maybeOutputPath

  -- Read metadata, sources and outputs.
  -- TODO: Don't fail if metadata and outputs are missing — just warn.
  let metadataPath = nbpartsDir </> "metadata.yaml"
  let sourcesPath = nbpartsDir </> "sources.yaml"
  let outputsPath = nbpartsDir </> "outputs.yaml"
  (sources :: [Nbparts.Source]) <- liftEither =<< liftIO (left Nbparts.PackParseSourcesError <$> Yaml.decodeFileEither sourcesPath)
  (metadata :: Nbparts.Metadata) <- liftEither =<< liftIO (left Nbparts.PackParseMetadataError <$> Yaml.decodeFileEither metadataPath)
  (unembeddedOutputs :: Nbparts.UnembeddedOutputs) <- liftEither =<< liftIO (left Nbparts.PackParseOutputsError <$> Yaml.decodeFileEither outputsPath)

  -- Create and export the notebook.
  let processNb :: (MonadError PackError m, MonadIO m, Aeson.ToJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> m ()
      processNb nb = do
        filledNb <- fillNotebook nbpartsDir sources metadata unembeddedOutputs nb
        liftIO $ exportNotebook outputPath filledNb

  let (Nbparts.Metadata major minor _ _) = metadata
  case major of
    3 -> processNb $ (emptyNotebook @Ipynb.NbV3) (major, minor)
    4 -> processNb $ (emptyNotebook @Ipynb.NbV4) (major, minor)
    _ -> throwError $ Nbparts.PackUnsupportedNotebookFormat (major, minor)

fillNotebook ::
  (MonadError PackError m, MonadIO m) =>
  FilePath ->
  [Nbparts.Source] ->
  Nbparts.Metadata ->
  Nbparts.UnembeddedOutputs ->
  Ipynb.Notebook a ->
  m (Ipynb.Notebook a)
fillNotebook nbpartsDir sources metadata unembeddedOutputs nb = do
  nbWithSources <- liftIO $ Nbparts.fillSources nbpartsDir nb sources
  nbWithSourcesAndMeta <- liftEither $ Nbparts.fillMetadata nbWithSources metadata
  Nbparts.fillOutputs nbpartsDir unembeddedOutputs nbWithSourcesAndMeta

prettyConfig :: AesonPretty.Config
prettyConfig = AesonPretty.defConfig {confIndent = AesonPretty.Spaces 1}

exportNotebook :: (Aeson.ToJSON (Ipynb.Notebook a)) => FilePath -> Ipynb.Notebook a -> IO ()
exportNotebook fp = LazyByteString.writeFile fp . AesonPretty.encodePretty' prettyConfig

emptyNotebook :: (Int, Int) -> Ipynb.Notebook a
emptyNotebook format = Ipynb.Notebook (Ipynb.JSONMeta Map.empty) format []
