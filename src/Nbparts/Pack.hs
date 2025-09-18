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
    outputPath :: Maybe FilePath,
    force :: Bool
  }

pack :: (MonadError PackError m, MonadIO m) => PackOptions -> m ()
pack opts = fmap (Maybe.fromMaybe ()) . runMaybeT $ do
  let outputPath = Maybe.fromMaybe (mkDefOutputPath opts.partsDirectory) opts.outputPath

  let mkImportPath :: FilePath -> Format -> FilePath
      mkImportPath fname fmt = opts.partsDirectory </> fname <.> formatExtension fmt

  -- Read manifest.
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

  -- Check if we should overwrite the output path if it already exists.
  cont <-
    liftIO $
      if opts.force
        then pure True
        else
          Directory.doesFileExist outputPath >>= \case
            True -> confirm $ "File \"" <> Text.pack outputPath <> "\" exists. Overwrite?"
            False -> pure True

  Monad.unless cont $ liftIO (Text.hPutStrLn stderr "Operation cancelled: file not overwritten")
  Monad.guard cont

  -- Read sources.
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

  -- Read metadata.
  let metadataPath = mkImportPath "metadata" metadataFormat
  (metadata :: NotebookMetadata) <- case metadataFormat of
    FormatYaml -> do
      res <- liftIO $ Yaml.decodeFileEither metadataPath
      liftEither $ left (PackParseYamlMetadataError . ParseYamlError) res
    FormatJson -> do
      res <- liftIO $ Aeson.eitherDecodeFileStrict metadataPath
      liftEither $ left (PackParseJsonMetadataError . Text.pack) res
    _ -> throwError $ PackIllegalFormatError IllegalFormatMetadata metadataFormat

  -- Read outputs.
  let outputsPath = mkImportPath "outputs" outputsFormat
  outputsPathExists <- liftIO $ Directory.doesFileExist outputsPath
  (unembeddedOutputs :: UnembeddedNotebookOutputs) <-
    if outputsPathExists
      then case outputsFormat of
        FormatYaml -> do
          res <- liftIO $ Yaml.decodeFileEither outputsPath
          liftEither $ left (PackParseYamlOutputsError . ParseYamlError) res
        FormatJson -> do
          res <- liftIO $ Aeson.eitherDecodeFileStrict outputsPath
          liftEither $ left (PackParseJsonOutputsError . Text.pack) res
        _ -> throwError $ PackIllegalFormatError IllegalFormatOutputs outputsFormat
      else do
        liftIO $ Text.hPutStrLn stderr "Warning: Could not find outputs file — assuming no outputs"
        pure mempty

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

  liftIO $ Text.putStrLn ("Packed \"" <> Text.pack opts.partsDirectory <> "\" into \"" <> Text.pack outputPath <> "\"")

mkDefOutputPath :: FilePath -> FilePath
mkDefOutputPath partsDir = case FilePath.stripExtension "nbparts" partsDir of
  Just stripped
    | "ipynb" `FilePath.isExtensionOf` stripped -> stripped
    | otherwise -> stripped <.> "ipynb"
  Nothing -> case FilePath.stripExtension "ipynb" partsDir of
    Just stripped -> stripped <> "-packed" <.> "ipynb"
    Nothing -> partsDir <.> "ipynb"

checkVersion :: (MonadError PackError m, MonadIO m) => Version -> m ()
checkVersion version = do
  let Version branch _ = version
      maybeMajorA = branch !? 0
      maybeMajorB = branch !? 1
      maybeMinor = branch !? 2
      maybePatch = branch !? 3
      maybeInvalid = branch !? 4 -- We want this to be Nothing.
  Monad.when (Maybe.isJust maybeInvalid) $ throwError (PackManifestUnknownVersionError version)

  (majorA, majorB, minor, patch) <- case (,,,) <$> maybeMajorA <*> maybeMajorB <*> maybeMinor <*> maybePatch of
    Just branches -> pure branches
    Nothing -> throwError $ PackManifestUnknownVersionError version

  liftIO $ compareVersion majorA majorB minor patch
  where
    Version cBranch' _ = currentNbpartsVersion
    cBranch = NonEmptyList.fromList cBranch'
    -- Safety: The known current nbparts version will always have a major, minor and patch version.
    -- The same cannot be said for the version parsed from the manifest, however.
    cMajorA = NonEmptyList.head cBranch
    cMajorB = cBranch NonEmptyList.!! 1
    cMinor = cBranch NonEmptyList.!! 2
    cPatch = cBranch NonEmptyList.!! 3

    compareVersion :: Int -> Int -> Int -> Int -> IO ()
    compareVersion majorA majorB minor patch
      | (cMajorA, cMajorB) > (majorA, majorB) = warnManifestVersion version Major Older
      | (cMajorA, cMajorB) < (majorA, majorB) = warnManifestVersion version Major Newer
      | cMinor < minor = warnManifestVersion version Minor Newer
      | cPatch < patch = warnManifestVersion version Patch Newer
      -- Newer minor or patch version should be backwards compatible.
      | otherwise = pure ()

data VersionComponent = Major | Minor | Patch

data VersionRelative = Older | Newer

warnManifestVersion :: Version -> VersionComponent -> VersionRelative -> IO ()
warnManifestVersion ver comp rel =
  Text.hPutStrLn stderr $
    "Warning: Manifest's "
      <> renderVersionComponent comp
      <> " version ("
      <> Text.pack (Version.showVersion ver)
      <> ") is "
      <> renderVersionRelative rel
      <> " than the current nbparts ("
      <> Text.pack (Version.showVersion currentNbpartsVersion)
      <> "). nbparts will still try to continue, "
      <> "but may fail or produce an incorrect notebook!"
  where
    renderVersionComponent :: VersionComponent -> Text
    renderVersionComponent Major = "major"
    renderVersionComponent Minor = "minor"
    renderVersionComponent Patch = "patch"

    renderVersionRelative :: VersionRelative -> Text
    renderVersionRelative Older = "older"
    renderVersionRelative Newer = "newer"

prettyConfig :: AesonPretty.Config
prettyConfig = AesonPretty.defConfig {confIndent = AesonPretty.Spaces 1}

exportJson :: (Aeson.ToJSON (a)) => FilePath -> a -> IO ()
exportJson fp = LazyByteString.writeFile fp . AesonPretty.encodePretty' prettyConfig

emptyNotebook :: (Int, Int) -> Ipynb.Notebook a
emptyNotebook format = Ipynb.Notebook (Ipynb.JSONMeta Map.empty) format []
