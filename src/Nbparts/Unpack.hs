module Nbparts.Unpack where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Control.Monad qualified as Monad
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (confIndent)
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Nbparts.Types qualified as Nbparts
import Nbparts.Types.Manifest qualified as Manifest
import Nbparts.Unpack.Metadata qualified as Nbparts
import Nbparts.Unpack.Outputs qualified as Nbparts
import Nbparts.Unpack.Sources qualified as Nbparts
import Nbparts.Unpack.Sources.Markdown qualified as Nbparts
import System.Directory qualified as Directory
import System.FilePath ((<.>), (</>))
import Text.Libyaml qualified as Libyaml

minNotebookFormat :: (Int, Int)
minNotebookFormat = (4, 0)

data UnpackOptions = UnpackOptions
  { notebook :: FilePath,
    sourcesFormat :: Nbparts.Format,
    metadataFormat :: Nbparts.Format,
    outputsFormat :: Nbparts.Format,
    outputPath :: Maybe FilePath
  }

unpack :: (MonadError Nbparts.UnpackError m, MonadIO m) => UnpackOptions -> m ()
unpack (UnpackOptions {notebook = notebookPath, sourcesFormat, metadataFormat, outputsFormat, outputPath}) = do
  let exportDirectory = Maybe.fromMaybe (notebookPath <.> "nbparts") outputPath
  let sourceMediaSubdir = "media"
  let outputMediaSubdir = "outputs-media"
  liftIO $ do
    Directory.createDirectoryIfMissing True (exportDirectory </> sourceMediaSubdir)
    Directory.createDirectoryIfMissing True exportDirectory
    Directory.createDirectoryIfMissing True (exportDirectory </> outputMediaSubdir)

  -- Parse the notebook.
  notebookBytes <- liftIO $ LazyByteString.readFile notebookPath
  (nb :: Nbparts.SomeNotebook) <-
    liftEither $
      left (Nbparts.UnpackParseNotebookError . T.pack) $
        Aeson.eitherDecode notebookBytes
  let withNb = Nbparts.withSomeNotebook nb

  -- Check notebook version.
  let format = withNb Nbparts.extractNotebookVersion
  Monad.when (format < minNotebookFormat) $ throwError (Nbparts.UnpackUnsupportedNotebookFormat format)

  -- Collect manifest, sources, metadata and outputs.
  let manifest =
        Manifest.defManifest
          { Manifest.sourcesFormat = sourcesFormat,
            Manifest.metadataFormat = metadataFormat,
            Manifest.outputsFormat = outputsFormat
          }
  metadata <- liftEither $ withNb Nbparts.collectMetadata
  sources <- withNb (Nbparts.collectSources exportDirectory sourceMediaSubdir)
  outputs <- withNb (liftEither . Nbparts.collectOutputs >=> liftIO . Nbparts.unembedOutputs exportDirectory outputMediaSubdir)

  -- Export manifest, sources, metadata and outputs.
  let yamlOptions = Yaml.setStringStyle nbpartsYamlStringStyle Yaml.defaultEncodeOptions
  let mkExportPath :: FilePath -> Nbparts.Format -> FilePath
      mkExportPath fname fmt = exportDirectory </> fname <.> Nbparts.formatExtension fmt

  let manifestPath = mkExportPath "nbparts" Nbparts.FormatYaml
  liftIO $ Yaml.encodeFile manifestPath manifest

  let sourcesPath = mkExportPath "sources" sourcesFormat
  case sourcesFormat of
    Nbparts.FormatYaml -> liftIO $ Yaml.encodeFileWith yamlOptions sourcesPath sources
    Nbparts.FormatJson -> liftIO $ exportJson sourcesPath sources
    Nbparts.FormatMarkdown -> do
      let lang = Maybe.fromMaybe "" $ extractLanguage metadata
      markdownText <- liftEither $ Nbparts.sourcesToMarkdown lang sources
      liftIO $ Text.writeFile sourcesPath markdownText

  let metadataPath = mkExportPath "metadata" metadataFormat
  liftIO $ case metadataFormat of
    Nbparts.FormatYaml -> Yaml.encodeFileWith yamlOptions metadataPath metadata
    Nbparts.FormatJson -> exportJson metadataPath metadata
    _ -> error $ "Illegal metadata format: " <> show metadataFormat

  let outputsPath = mkExportPath "outputs" outputsFormat
  liftIO $ case outputsFormat of
    Nbparts.FormatYaml -> Yaml.encodeFileWith yamlOptions outputsPath outputs
    Nbparts.FormatJson -> exportJson outputsPath outputs
    _ -> error $ "Illegal outputs format: " <> show outputsFormat

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

extractLanguage :: Nbparts.NotebookMetadata -> Maybe T.Text
extractLanguage (Nbparts.NotebookMetadata _ _ (Ipynb.JSONMeta nbMeta) _) = do
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
