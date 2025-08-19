module Nbparts.Pack.Error where

import Data.Text (Text)
import Data.Yaml qualified as Yaml
import Nbparts.Types qualified as Nbparts
import Text.Megaparsec qualified as Megaparsec
import qualified Data.Text as Text

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
  | PackCellMetadataTypeMismatch {expected :: Nbparts.CellMetadataTag, actual :: Nbparts.CellMetadataTag}

newtype ParseMarkdownSourcesError = ParseMarkdownSourcesJsonError Text
  deriving (Show, Eq, Ord)

instance Megaparsec.ShowErrorComponent ParseMarkdownSourcesError where
  showErrorComponent (ParseMarkdownSourcesJsonError msg) =
    "Invalid JSON in nbparts cell: " <> Text.unpack msg
