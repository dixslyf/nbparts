module Nbparts.Pack.Error where

import Data.Text (Text)
import Data.Yaml qualified as Yaml
import Nbparts.Types qualified as Nbparts
import Text.Pandoc (PandocError)

data PackError
  = PackPandocError PandocError
  | PackParseIpynbError Text
  | PackParseMetadataError Yaml.ParseException
  | PackParseOutputsError Yaml.ParseException
  | PackMissingCellIdError
  | PackMissingCellMetadataError Text
  | PackMissingCellOutputsError Text
  | PackCellMetadataTypeMismatch {expected :: Nbparts.CellMetadataTag, actual :: Nbparts.CellMetadataTag}
