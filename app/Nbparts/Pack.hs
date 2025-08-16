module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
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

pack :: FilePath -> Maybe FilePath -> IO (Either PackError ())
pack nbpartsDir maybeOutputPath = runExceptT $ do
  -- `nbpartsDir` should be in the form "some_notebook.ipynb.nbparts".
  let fallbackOutputPath = FilePath.dropExtension nbpartsDir
  let outputPath = Maybe.fromMaybe fallbackOutputPath maybeOutputPath

  -- Read sources.
  let sourcesPath = nbpartsDir </> "sources.yaml"
  (sources :: [Nbparts.Source]) <- ExceptT (left Nbparts.PackParseSourcesError <$> Yaml.decodeFileEither sourcesPath)

  -- Read metadata.
  -- TODO: Don't fail if file is missing — just warn.
  let metadataPath = nbpartsDir </> "metadata.yaml"
  (metadata :: Nbparts.Metadata) <- ExceptT (left Nbparts.PackParseMetadataError <$> Yaml.decodeFileEither metadataPath)

  -- Read outputs.
  let outputsPath = nbpartsDir </> "outputs.yaml"
  (unembeddedOutputs :: Nbparts.UnembeddedOutputs) <- ExceptT (left Nbparts.PackParseOutputsError <$> Yaml.decodeFileEither outputsPath)

  -- TODO: Read notebook format.
  let nb = (emptyNotebook @Ipynb.NbV4) (4, 5)
  nbWithSources <- liftIO $ Nbparts.fillSources nbpartsDir nb sources
  nbWithSourcesAndMeta <- ExceptT $ pure $ Nbparts.fillMetadata nbWithSources metadata
  nbWithSourcesMetaAndOutputs <- ExceptT $ Nbparts.fillOutputs nbpartsDir unembeddedOutputs nbWithSourcesAndMeta
  liftIO $ exportNotebook outputPath nbWithSourcesMetaAndOutputs

prettyConfig :: AesonPretty.Config
prettyConfig = AesonPretty.defConfig {confIndent = AesonPretty.Spaces 1}

exportNotebook :: (Aeson.ToJSON (Ipynb.Notebook a)) => FilePath -> Ipynb.Notebook a -> IO ()
exportNotebook fp = LazyByteString.writeFile fp . AesonPretty.encodePretty' prettyConfig

emptyNotebook :: (Int, Int) -> Ipynb.Notebook a
emptyNotebook format = Ipynb.Notebook (Ipynb.JSONMeta Map.empty) format []
