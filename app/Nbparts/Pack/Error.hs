module Nbparts.Pack.Error where

import Data.Text (Text)
import Data.Yaml qualified as Yaml
import Text.Pandoc (PandocError)

data PackError
  = PackPandocError PandocError
  | PackParseIpynbError Text
  | PackParseMetadataError Yaml.ParseException
  | PackMissingCellIdError
  | PackMissingCellMetadataError Text
