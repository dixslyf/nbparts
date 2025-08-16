module Nbparts.Unpack.Error where

import Data.Text (Text)

data UnpackError
  = UnpackJSONDecodeError Text
  | UnpackMissingCellIdError
