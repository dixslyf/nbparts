module Nbparts.Types.Error where

import Commonmark qualified
import Control.Exception qualified as Exception
import Data.Ord qualified as Ord
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version qualified as Data
import Data.Version qualified as Version
import Data.Yaml qualified as Yaml
import Nbparts.Types.Manifest qualified as Manifest
import Text.Megaparsec qualified as Megaparsec
import Text.Parsec (errorPos)

recommendedNotebookFormat :: (Int, Int)
recommendedNotebookFormat = (4, 5)

data NbpartsError = UnpackError UnpackError | PackError PackError
  deriving (Show, Eq)

data UnpackError
  = UnpackParseNotebookError Text
  | UnpackParseMarkdownError Commonmark.ParseError
  | UnpackUnsupportedNotebookFormat (Int, Int)
  | UnpackMissingCellIdError
  deriving (Show, Eq)

data PackError
  = PackUnsupportedNotebookFormat (Int, Int)
  | PackParseManifestError ParseYamlError
  | PackManifestUnknownVersionError Data.Version
  | PackIllegalFormatError IllegalFormatContext Manifest.Format
  | PackParseYamlSourcesError ParseYamlError
  | PackParseJsonSourcesError Text
  | PackParseMarkdownSourcesError (Megaparsec.ParseErrorBundle Text ParseMarkdownSourcesError)
  | PackParseYamlMetadataError ParseYamlError
  | PackParseJsonMetadataError Text
  | PackParseYamlOutputsError ParseYamlError
  | PackParseJsonOutputsError Text
  | PackMissingCellIdError
  | PackCellMetadataTypeMismatch {expected :: CellMetadataTag, actual :: CellMetadataTag}
  deriving (Show, Eq)

-- Wrapper so that we can make an `Eq` instance.
newtype ParseYamlError = ParseYamlError Yaml.ParseException
  deriving (Show)

-- The author of `yaml` performs equality checks with the `Show` instance as well,
-- so this should be fine. https://github.com/snoyberg/yaml/issues/189
instance Eq ParseYamlError where
  a == b = show a == show b

data IllegalFormatContext = IllegalFormatSources | IllegalFormatMetadata | IllegalFormatOutputs
  deriving (Show, Eq, Ord)

data ParseMarkdownSourcesError
  = ParseMarkdownSourcesJsonError Text
  | ParseMarkdownSourcesMarkdownError Commonmark.ParseError
  deriving (Show, Eq)

instance Ord ParseMarkdownSourcesError where
  compare (ParseMarkdownSourcesMarkdownError mdErr1) (ParseMarkdownSourcesMarkdownError mdErr2) =
    compare (errorPos mdErr1) (errorPos mdErr2)
  compare (ParseMarkdownSourcesJsonError t1) (ParseMarkdownSourcesJsonError t2) = compare t1 t2
  compare _ _ = Ord.EQ

data CellMetadataTag = CodeCellMetadataTag | GenericCellMetadataTag
  deriving (Show, Eq, Ord)

instance Megaparsec.ShowErrorComponent ParseMarkdownSourcesError where
  showErrorComponent (ParseMarkdownSourcesJsonError msg) =
    "Invalid JSON in nbparts cell marker: " <> Text.unpack msg
  showErrorComponent (ParseMarkdownSourcesMarkdownError mdErr) =
    "Failed to parse markdown: " <> show mdErr

renderError :: NbpartsError -> Text
renderError err = case err of
  UnpackError (UnpackParseNotebookError message) -> "Failed to parse notebook: " <> message
  UnpackError (UnpackParseMarkdownError mdErr) -> "Failed to parse markdown: " <> Text.pack (show mdErr)
  UnpackError (UnpackUnsupportedNotebookFormat (major, minor)) ->
    "Unsupported notebook format: "
      <> Text.pack (show major)
      <> "."
      <> Text.pack (show minor)
  UnpackError UnpackMissingCellIdError ->
    "Notebook contains cell(s) without an identifier. Try upgrading your notebook to at least version "
      <> Text.pack (show $ fst recommendedNotebookFormat)
      <> "."
      <> Text.pack (show $ snd recommendedNotebookFormat)
      <> "."
  PackError (PackUnsupportedNotebookFormat (major, minor)) ->
    "Unsupported notebook format: "
      <> Text.pack (show major)
      <> "."
      <> Text.pack (show minor)
  PackError (PackParseManifestError (ParseYamlError ex)) -> "Failed to parse manifest: " <> Text.pack (Exception.displayException ex)
  PackError (PackManifestUnknownVersionError version) -> "Unknown manifest version: " <> Text.pack (Version.showVersion version)
  PackError (PackIllegalFormatError ctx fmt) -> "Illegal format for " <> renderIllegalFormatContext ctx <> ":" <> renderFormat fmt
  PackError (PackParseYamlSourcesError (ParseYamlError ex)) -> "Failed to parse sources: " <> Text.pack (Exception.displayException ex)
  PackError (PackParseJsonSourcesError parseErr) -> "Failed to parse sources: " <> parseErr
  PackError (PackParseMarkdownSourcesError errBundle) -> Text.pack $ Megaparsec.errorBundlePretty errBundle
  PackError (PackParseYamlMetadataError (ParseYamlError ex)) -> "Failed to parse metadata: " <> Text.pack (Exception.displayException ex)
  PackError (PackParseJsonMetadataError parseErr) -> "Failed to parse metadata: " <> parseErr
  PackError (PackParseYamlOutputsError (ParseYamlError ex)) -> "Failed to parse outputs: " <> Text.pack (Exception.displayException ex)
  PackError (PackParseJsonOutputsError parseErr) -> "Failed to parse outputs: " <> parseErr
  PackError PackMissingCellIdError -> "Markdown content contains missing cell ID"
  PackError (PackCellMetadataTypeMismatch expected actual) ->
    "Cell metadata type mismatch. Expected: "
      <> renderCellMetadataTag expected
      <> ", but got: "
      <> renderCellMetadataTag actual

renderFormat :: Manifest.Format -> Text
renderFormat Manifest.FormatYaml = "yaml"
renderFormat Manifest.FormatJson = "json"
renderFormat Manifest.FormatMarkdown = "markdown"

renderIllegalFormatContext :: IllegalFormatContext -> Text
renderIllegalFormatContext IllegalFormatSources = "sources"
renderIllegalFormatContext IllegalFormatMetadata = "metadata"
renderIllegalFormatContext IllegalFormatOutputs = "outputs"

renderCellMetadataTag :: CellMetadataTag -> Text
renderCellMetadataTag CodeCellMetadataTag = "code cell metadata"
renderCellMetadataTag GenericCellMetadataTag = "generic cell metadata"
