module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Control.Monad qualified as Monad
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (confIndent))
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Ipynb qualified as Ipynb
import Data.List ((!?))
import Data.List.NonEmpty qualified as NonEmptyList
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (Version (Version))
import Data.Version qualified as Version
import Data.Yaml qualified as Yaml
import Nbparts.Pack.Metadata (fillMetadata)
import Nbparts.Pack.Outputs (fillOutputs)
import Nbparts.Pack.Sources (fillSources)
import Nbparts.Pack.Sources.Markdown (markdownToSources)
import Nbparts.Types
  ( CellSource,
    Format (FormatJson, FormatMarkdown, FormatYaml),
    IllegalFormatContext (IllegalFormatMetadata, IllegalFormatOutputs),
    Manifest (Manifest, metadataFormat, nbpartsVersion, outputsFormat, sourcesFormat),
    NotebookMetadata (NotebookMetadata),
    PackError
      ( PackIllegalFormatError,
        PackManifestTooNewError,
        PackManifestTooOldError,
        PackManifestUnknownVersionError,
        PackParseJsonMetadataError,
        PackParseJsonOutputsError,
        PackParseJsonSourcesError,
        PackParseManifestError,
        PackParseYamlMetadataError,
        PackParseYamlOutputsError,
        PackParseYamlSourcesError,
        PackUnsupportedNotebookFormat
      ),
    ParseYamlError (ParseYamlError),
    SomeNotebook (SomeNotebook),
    UnembeddedNotebookOutputs,
    currentNbpartsVersion,
    formatExtension,
    withSomeNotebook,
  )
import Nbparts.Util.Prompt (confirm)
import System.Directory qualified as Directory
import System.FilePath ((<.>), (</>))
import System.FilePath qualified as FilePath
import System.IO (stderr)

data PackOptions = PackOptions
  { partsDirectory :: FilePath,
    outputPath :: Maybe FilePath
  }

pack :: (MonadError PackError m, MonadIO m) => PackOptions -> m ()
pack opts = fmap (Maybe.fromMaybe ()) . runMaybeT $ do
  let outputPath = Maybe.fromMaybe (mkDefOutputPath opts.partsDirectory) opts.outputPath

  -- Check if we should overwrite the output path if it already exists.
  cont <-
    liftIO $
      Directory.doesFileExist outputPath >>= \case
        True -> confirm $ "File \"" <> Text.pack outputPath <> "\" exists. Overwrite?"
        False -> pure True

  Monad.unless cont $ liftIO (Text.hPutStrLn stderr "Operation cancelled: file not overwritten")
  Monad.guard cont

  -- Read manifest, metadata, sources and outputs.
  let mkImportPath :: FilePath -> Format -> FilePath
      mkImportPath fname fmt = opts.partsDirectory </> fname <.> formatExtension fmt

  let manifestPath = mkImportPath "nbparts" FormatYaml
  ( Manifest
      { nbpartsVersion,
        sourcesFormat,
        metadataFormat,
        outputsFormat
      }
    ) <-
    liftEither
      =<< liftIO
        ( left
            (PackParseManifestError . ParseYamlError)
            <$> Yaml.decodeFileEither manifestPath
        )

  checkVersion nbpartsVersion

  -- TODO: Don't fail if metadata and outputs are missing — just warn.
  let sourcesPath = mkImportPath "sources" sourcesFormat
  (sources :: [CellSource]) <- case sourcesFormat of
    FormatYaml -> do
      res <- liftIO $ Yaml.decodeFileEither sourcesPath
      liftEither $ left (PackParseYamlSourcesError . ParseYamlError) res
    FormatJson -> do
      res <- liftIO $ Aeson.eitherDecodeFileStrict sourcesPath
      liftEither $ left (PackParseJsonSourcesError . Text.pack) res
    FormatMarkdown -> do
      mdText <- liftIO $ Text.readFile sourcesPath
      liftEither $ markdownToSources sourcesPath mdText

  let metadataPath = mkImportPath "metadata" metadataFormat
  (metadata :: NotebookMetadata) <- case metadataFormat of
    FormatYaml -> do
      res <- liftIO $ Yaml.decodeFileEither metadataPath
      liftEither $ left (PackParseYamlMetadataError . ParseYamlError) res
    FormatJson -> do
      res <- liftIO $ Aeson.eitherDecodeFileStrict metadataPath
      liftEither $ left (PackParseJsonMetadataError . Text.pack) res
    _ -> throwError $ PackIllegalFormatError IllegalFormatMetadata metadataFormat

  let outputsPath = mkImportPath "outputs" outputsFormat
  (unembeddedOutputs :: UnembeddedNotebookOutputs) <- case outputsFormat of
    FormatYaml -> do
      res <- liftIO $ Yaml.decodeFileEither outputsPath
      liftEither $ left (PackParseYamlOutputsError . ParseYamlError) res
    FormatJson -> do
      res <- liftIO $ Aeson.eitherDecodeFileStrict outputsPath
      liftEither $ left (PackParseJsonOutputsError . Text.pack) res
    _ -> throwError $ PackIllegalFormatError IllegalFormatOutputs outputsFormat

  let (NotebookMetadata major minor _ _) = metadata
  nb <- case major of
    4 -> pure $ SomeNotebook $ (emptyNotebook @Ipynb.NbV4) (major, minor)
    _ -> throwError $ PackUnsupportedNotebookFormat (major, minor)

  -- Create and export the notebook.
  filledNb <-
    withSomeNotebook
      nb
      ( liftIO . fillSources opts.partsDirectory sources
          >=> liftEither . fillMetadata metadata
          >=> fillOutputs opts.partsDirectory unembeddedOutputs
          >=> pure . SomeNotebook
      )

  liftIO $ exportJson outputPath filledNb

mkDefOutputPath :: FilePath -> FilePath
mkDefOutputPath partsDir = case FilePath.stripExtension "nbparts" partsDir of
  Just stripped
    | "ipynb" `FilePath.isExtensionOf` stripped -> stripped
    | otherwise -> stripped <.> "ipynb"
  Nothing -> case FilePath.stripExtension "ipynb" partsDir of
    Just stripped -> stripped <> "-packed" <.> "ipynb"
    Nothing -> partsDir <.> "ipynb"

checkVersion :: (MonadError PackError m, MonadIO m) => Version -> m ()
checkVersion nbpartsVersion = do
  let Version branch _ = nbpartsVersion
      maybeMajorA = branch !? 0
      maybeMajorB = branch !? 1
      maybeMinor = branch !? 2
      maybePatch = branch !? 3

  (majorA, majorB, minor, patch) <- case (,,,) <$> maybeMajorA <*> maybeMajorB <*> maybeMinor <*> maybePatch of
    Just branches -> pure branches
    Nothing -> throwError $ PackManifestUnknownVersionError nbpartsVersion

  compareVersion majorA majorB minor patch
  where
    Version cBranch' _ = currentNbpartsVersion
    cBranch = NonEmptyList.fromList cBranch'
    -- Safety: The known current nbparts version will always have a major, minor and patch version.
    -- The same cannot be said for the version parsed from the manifest, however.
    cMajorA = NonEmptyList.head cBranch
    cMajorB = cBranch NonEmptyList.!! 1
    cMinor = cBranch NonEmptyList.!! 2
    cPatch = cBranch NonEmptyList.!! 3

    compareVersion :: (MonadError PackError m, MonadIO m) => Int -> Int -> Int -> Int -> m ()
    compareVersion majorA majorB minor patch
      | (cMajorA, cMajorB) > (majorA, majorB) = throwError $ PackManifestTooOldError nbpartsVersion
      | (cMajorA, cMajorB) < (majorA, majorB) = throwError $ PackManifestTooNewError nbpartsVersion
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
