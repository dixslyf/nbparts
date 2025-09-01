module Nbparts.Pack where

import Control.Applicative (liftA3)
import Control.Arrow (left)
import Control.Monad ((>=>))
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (confIndent))
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Ipynb qualified as Ipynb
import Data.List ((!?))
import Data.List.NonEmpty qualified as NonEmptyList
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (Version (Version))
import Data.Version qualified as Version
import Data.Yaml qualified as Yaml
import Nbparts.Pack.Metadata qualified as Nbparts
import Nbparts.Pack.Outputs qualified as Nbparts
import Nbparts.Pack.Sources qualified as Nbparts
import Nbparts.Pack.Sources.Markdown qualified as Nbparts
import Nbparts.Types (currentNbpartsVersion)
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
      { nbpartsVersion,
        sourcesFormat,
        metadataFormat,
        outputsFormat
      }
    ) <-
    liftEither =<< liftIO (left Nbparts.PackParseManifestError <$> Yaml.decodeFileEither manifestPath)

  checkVersion nbpartsVersion

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

checkVersion :: (MonadError Nbparts.PackError m, MonadIO m) => Version -> m ()
checkVersion nbpartsVersion = do
  let Version branch _ = nbpartsVersion
      maybeMajor = branch !? 0
      maybeMinor = branch !? 1
      maybePatch = branch !? 2

  (major, minor, patch) <- case liftA3 (,,) maybeMajor maybeMinor maybePatch of
    Just branches -> pure branches
    Nothing -> throwError $ Nbparts.PackManifestUnknownVersionError nbpartsVersion

  compareVersion major minor patch
  where
    Version cBranch' _ = currentNbpartsVersion
    cBranch = NonEmptyList.fromList cBranch'
    -- Safety: The known current nbparts version will always have a major, minor and patch version.
    -- The same cannot be said for the version parsed from the manifest, however.
    cMajor = NonEmptyList.head cBranch
    cMinor = cBranch NonEmptyList.!! 1
    cPatch = cBranch NonEmptyList.!! 2

    compareVersion :: (MonadError Nbparts.PackError m, MonadIO m) => Int -> Int -> Int -> m ()
    compareVersion major minor patch
      | cMajor > major = throwError $ Nbparts.PackManifestTooOldError nbpartsVersion
      | cMajor < major = throwError $ Nbparts.PackManifestTooNewError nbpartsVersion
      | cMinor < minor = liftIO $ warnManifestNewer "minor"
      | cPatch < patch = liftIO $ warnManifestNewer "patch"
      -- Newer minor or patch version should be backwards compatible.
      | otherwise = pure ()

    warnManifestNewer :: String -> IO ()
    warnManifestNewer compStr =
      putStrLn $
        "Warning: Manifest's "
          <> compStr
          <> " version ("
          <> Version.showVersion nbpartsVersion
          <> ") is newer than the current nbparts ("
          <> Version.showVersion currentNbpartsVersion
          <> "). nbparts will still try to continue, "
          <> "but may fail or produce an incorrect notebook!"

prettyConfig :: AesonPretty.Config
prettyConfig = AesonPretty.defConfig {confIndent = AesonPretty.Spaces 1}

exportJson :: (Aeson.ToJSON (a)) => FilePath -> a -> IO ()
exportJson fp = LazyByteString.writeFile fp . AesonPretty.encodePretty' prettyConfig

emptyNotebook :: (Int, Int) -> Ipynb.Notebook a
emptyNotebook format = Ipynb.Notebook (Ipynb.JSONMeta Map.empty) format []
