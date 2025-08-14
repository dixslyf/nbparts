module Nbparts.Unpack.Error where

import Data.Text (Text)
import Text.Pandoc (PandocError)

data UnpackError
  = UnpackJSONDecodeError Text
  | UnpackMissingCellIdError
  | UnpackPandocError PandocError
