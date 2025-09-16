module Nbparts.Unpack where

import Control.Arrow (left)
import Control.Monad qualified as Monad
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (confIndent)
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Nbparts.Types
  ( Format (FormatJson, FormatMarkdown, FormatYaml),
    NotebookMetadata (NotebookMetadata),
    SomeNotebook,
    UnpackError (UnpackParseNotebookError, UnpackUnsupportedNotebookFormat),
    defManifest,
    formatExtension,
    withSomeNotebook,
  )
import Nbparts.Types.Manifest qualified as Manifest
import Nbparts.Unpack.Metadata (collectMetadata, extractNotebookVersion)
import Nbparts.Unpack.Outputs (collectOutputs)
import Nbparts.Unpack.Sources (collectSources)
import Nbparts.Unpack.Sources.Markdown (sourcesToMarkdown)
import Nbparts.Util.Prompt (confirm)
import System.Directory qualified as Directory
import System.FilePath ((<.>), (</>))
import System.IO (stderr)
import Text.Libyaml qualified as Libyaml

minNotebookFormat :: (Int, Int)
minNotebookFormat = (4, 0)

data UnpackOptions = UnpackOptions
  { notebookPath :: FilePath,
    sourcesFormat :: Format,
    metadataFormat :: Format,
    outputsFormat :: Format,
    outputPath :: Maybe FilePath
  }

unpack :: (MonadError UnpackError m, MonadIO m) => UnpackOptions -> m ()
unpack opts = fmap (Maybe.fromMaybe ()) . runMaybeT $ do
  let exportDirectory = Maybe.fromMaybe (mkDefOutputPath opts.notebookPath) opts.outputPath

  -- Check if we should overwrite the export directory (if it already exists and is non-empty).
  cont <-
    liftIO $
      shouldConfirmOverwrite exportDirectory >>= \case
        True -> confirm $ "Directory \"" <> Text.pack exportDirectory <> "\" exists and is not empty. Overwrite?"
        False -> pure True

  Monad.unless cont $ liftIO (Text.hPutStrLn stderr "Operation cancelled: directory not overwritten")
  Monad.guard cont

  let sourceMediaSubdir = "media"
  let outputMediaSubdir = "outputs-media"
  liftIO $ do
    Directory.createDirectoryIfMissing True (exportDirectory </> sourceMediaSubdir)
    Directory.createDirectoryIfMissing True exportDirectory
    Directory.createDirectoryIfMissing True (exportDirectory </> outputMediaSubdir)

  -- Parse the notebook.
  notebookBytes <- liftIO $ LazyByteString.readFile opts.notebookPath
  (nb :: SomeNotebook) <-
    liftEither $
      left (UnpackParseNotebookError . T.pack) $
        Aeson.eitherDecode notebookBytes
  let withNb = withSomeNotebook nb

  -- Check notebook version.
  let format = withNb extractNotebookVersion
  Monad.when (format < minNotebookFormat) $ throwError (UnpackUnsupportedNotebookFormat format)

  -- Collect manifest, sources, metadata and outputs.
  let manifest =
        defManifest
          { Manifest.sourcesFormat = opts.sourcesFormat,
            Manifest.metadataFormat = opts.metadataFormat,
            Manifest.outputsFormat = opts.outputsFormat
          }
  metadata <- liftEither $ withNb collectMetadata
  (sources, sourceMedia) <- liftEither $ withNb (collectSources sourceMediaSubdir)
  (outputs, outputMedia) <- liftEither $ withNb (collectOutputs outputMediaSubdir)

  -- Export manifest, sources, metadata and outputs.
  let yamlOptions = Yaml.setStringStyle nbpartsYamlStringStyle Yaml.defaultEncodeOptions
  let mkExportPath :: FilePath -> Format -> FilePath
      mkExportPath fname fmt = exportDirectory </> fname <.> formatExtension fmt

  let manifestPath = mkExportPath "nbparts" FormatYaml
  liftIO $ Yaml.encodeFile manifestPath manifest

  let sourcesPath = mkExportPath "sources" opts.sourcesFormat
  case opts.sourcesFormat of
    FormatYaml -> liftIO $ Yaml.encodeFileWith yamlOptions sourcesPath sources
    FormatJson -> liftIO $ exportJson sourcesPath sources
    FormatMarkdown -> do
      let lang = Maybe.fromMaybe "" $ extractLanguage metadata
      markdownText <- liftEither $ sourcesToMarkdown lang sources
      liftIO $ Text.writeFile sourcesPath markdownText
  liftIO $ mapM_ (\(path, bytes) -> ByteString.writeFile (exportDirectory </> path) bytes) sourceMedia

  let metadataPath = mkExportPath "metadata" opts.metadataFormat
  liftIO $ case opts.metadataFormat of
    FormatYaml -> Yaml.encodeFileWith yamlOptions metadataPath metadata
    FormatJson -> exportJson metadataPath metadata
    _ -> error $ "Illegal metadata format: " <> show opts.metadataFormat

  let outputsPath = mkExportPath "outputs" opts.outputsFormat
  liftIO $ case opts.outputsFormat of
    FormatYaml -> Yaml.encodeFileWith yamlOptions outputsPath outputs
    FormatJson -> exportJson outputsPath outputs
    _ -> error $ "Illegal outputs format: " <> show opts.outputsFormat
  liftIO $ mapM_ (\(path, bytes) -> ByteString.writeFile (exportDirectory </> path) bytes) outputMedia

  liftIO $ Text.putStrLn ("Unpacked \"" <> Text.pack opts.notebookPath <> "\" to \"" <> Text.pack exportDirectory <> "\"")

shouldConfirmOverwrite :: FilePath -> IO Bool
shouldConfirmOverwrite exportDirectory = do
  exists <- Directory.doesDirectoryExist exportDirectory
  if exists
    then
      -- Check that the directory is not empty.
      not . null <$> Directory.listDirectory exportDirectory
    else
      pure False

mkDefOutputPath :: FilePath -> FilePath
mkDefOutputPath nbPath = nbPath <.> "nbparts"

hasOnlyOneNewline :: T.Text -> Bool
hasOnlyOneNewline text = T.length (T.filter (== '\n') text) == 1

hasNewlineSuffix :: T.Text -> Bool
hasNewlineSuffix = T.isSuffixOf "\n"

-- Based on Yaml's default string style.
nbpartsYamlStringStyle :: T.Text -> (Libyaml.Tag, Libyaml.Style)
nbpartsYamlStringStyle s
  | hasOnlyOneNewline s && hasNewlineSuffix s = (Libyaml.NoTag, Libyaml.DoubleQuoted)
  | "\n" `T.isInfixOf` s = (Libyaml.NoTag, Libyaml.Literal)
  | Yaml.isSpecialString s = (Libyaml.NoTag, Libyaml.SingleQuoted)
  | otherwise = (Libyaml.NoTag, Libyaml.PlainNoTag)

extractLanguage :: NotebookMetadata -> Maybe T.Text
extractLanguage (NotebookMetadata _ _ (Ipynb.JSONMeta nbMeta) _) = do
  kernelspec <- Map.lookup "kernelspec" nbMeta
  langFromKernelSpec kernelspec

langFromKernelSpec :: Aeson.Value -> Maybe T.Text
langFromKernelSpec (Aeson.Object obj) = case Aeson.KeyMap.lookup "language" obj of
  Just (Aeson.String lang) -> Just lang
  _ -> Nothing
langFromKernelSpec _ = Nothing

exportJson :: (Aeson.ToJSON (a)) => FilePath -> a -> IO ()
exportJson fp = LazyByteString.writeFile fp . AesonPretty.encodePretty' aesonPrettyConfig

aesonPrettyConfig :: AesonPretty.Config
aesonPrettyConfig = AesonPretty.defConfig {confIndent = AesonPretty.Spaces 2}
