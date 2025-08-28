module Nbparts.Types.Error where

import Control.Exception qualified as Exception
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Text.Megaparsec qualified as Megaparsec

recommendedNotebookFormat :: (Int, Int)
recommendedNotebookFormat = (4, 5)

data Error = UnpackError UnpackError | PackError PackError
  deriving (Show, Eq)

data UnpackError
  = UnpackParseNotebookError Text
  | UnpackUnsupportedNotebookFormat (Int, Int)
  | UnpackMissingCellIdError
  | UnpackMissingCellAttachmentError {cellId :: Text, attachmentName :: Text}
  deriving (Show, Eq)

data PackError
  = PackUnsupportedNotebookFormat (Int, Int)
  | PackParseManifestError Yaml.ParseException
  | PackParseYamlSourcesError Yaml.ParseException
  | PackParseMarkdownSourcesError (Megaparsec.ParseErrorBundle Text ParseMarkdownSourcesError)
  | PackParseMetadataError Yaml.ParseException
  | PackParseOutputsError Yaml.ParseException
  | PackMissingCellIdError
  | PackMissingCellMetadataError Text
  | PackMissingCellOutputsError Text
  | PackCellMetadataTypeMismatch {expected :: CellMetadataTag, actual :: CellMetadataTag}
  deriving (Show)

instance Eq PackError where
  (==) a b = show a == show b
  (/=) a b = show a /= show b

newtype ParseMarkdownSourcesError = ParseMarkdownSourcesJsonError Text
  deriving (Show, Eq, Ord)

data CellMetadataTag = CodeCellMetadataTag | GenericCellMetadataTag
  deriving (Show, Eq, Ord)

instance Megaparsec.ShowErrorComponent ParseMarkdownSourcesError where
  showErrorComponent (ParseMarkdownSourcesJsonError msg) =
    "Invalid JSON in nbparts cell: " <> Text.unpack msg

renderError :: Error -> Text
renderError err = case err of
  UnpackError (UnpackParseNotebookError message) -> "Failed to parse notebook: " <> message
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
  UnpackError (UnpackMissingCellAttachmentError cellId attachmentName) ->
    "Could not find attachment \""
      <> attachmentName
      <> "\" for cell \""
      <> cellId
      <> "\""
  PackError (PackUnsupportedNotebookFormat (major, minor)) ->
    "Unsupported notebook format: "
      <> Text.pack (show major)
      <> "."
      <> Text.pack (show minor)
  PackError (PackParseManifestError parseErr) -> "Failed to parse manifest: " <> Text.pack (Exception.displayException parseErr)
  PackError (PackParseYamlSourcesError parseErr) -> "Failed to parse sources: " <> Text.pack (Exception.displayException parseErr)
  PackError (PackParseMarkdownSourcesError errBundle) -> Text.pack $ Megaparsec.errorBundlePretty errBundle
  PackError (PackParseMetadataError parseErr) -> "Failed to parse metadata: " <> Text.pack (Exception.displayException parseErr)
  PackError (PackParseOutputsError parseErr) -> "Failed to parse outputs: " <> Text.pack (Exception.displayException parseErr)
  PackError PackMissingCellIdError -> "Markdown content contains missing cell ID"
  PackError (PackMissingCellMetadataError cellId) -> "Could not find metadata for cell ID: " <> cellId
  PackError (PackMissingCellOutputsError cellId) -> "Could not find outputs for cell ID: " <> cellId
  PackError (PackCellMetadataTypeMismatch expected actual) ->
    "Cell metadata type mismatch. Expected: "
      <> renderCellMetadataTag expected
      <> ", but got: "
      <> renderCellMetadataTag actual

renderCellMetadataTag :: CellMetadataTag -> Text
renderCellMetadataTag CodeCellMetadataTag = "code cell metadata"
renderCellMetadataTag GenericCellMetadataTag = "generic cell metadata"
