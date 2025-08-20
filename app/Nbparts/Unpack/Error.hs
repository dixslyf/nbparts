module Nbparts.Unpack.Error where

import Data.Text (Text)

data UnpackError
  = UnpackJSONDecodeError Text
  | UnpackUnsupportedNotebookFormat (Int, Int)
  | UnpackMissingCellIdError
  | UnpackMissingCellAttachmentError {cellId :: Text, attachmentName :: Text}
