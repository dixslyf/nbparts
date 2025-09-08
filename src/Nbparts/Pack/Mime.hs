module Nbparts.Pack.Mime where

import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.Ipynb qualified as Ipynb
import Nbparts.Types
  ( UnembeddedMimeAttachments (UnembeddedMimeAttachments),
    UnembeddedMimeBundle (UnembeddedMimeBundle),
    UnembeddedMimeData (BinaryData, JsonData, TextualData),
  )
import System.FilePath ((</>))

embedMimeAttachments :: FilePath -> UnembeddedMimeAttachments -> IO Ipynb.MimeAttachments
embedMimeAttachments readDir (UnembeddedMimeAttachments unembedded) =
  coerce $
    traverse
      (embedMimeBundle readDir)
      unembedded

embedMimeBundle :: FilePath -> UnembeddedMimeBundle -> IO Ipynb.MimeBundle
embedMimeBundle readDir = coerce (fmap Ipynb.MimeBundle . traverse (embedMimeData readDir))

embedMimeData :: FilePath -> UnembeddedMimeData -> IO Ipynb.MimeData
embedMimeData readDir (BinaryData path) = do
  bytes <- ByteString.readFile $ readDir </> path
  return $ Ipynb.BinaryData bytes
embedMimeData _ (TextualData text) = pure $ Ipynb.TextualData text
embedMimeData _ (JsonData value) = pure $ Ipynb.JsonData value
