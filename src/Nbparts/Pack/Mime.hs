module Nbparts.Pack.Mime where

import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.Ipynb qualified as Ipynb
import Nbparts.Types qualified as Nbparts
import System.FilePath ((</>))

embedMimeAttachments :: FilePath -> Nbparts.UnembeddedMimeAttachments -> IO Ipynb.MimeAttachments
embedMimeAttachments readDir (Nbparts.UnembeddedMimeAttachments unembedded) =
  coerce $
    traverse
      (embedMimeBundle readDir)
      unembedded

embedMimeBundle :: FilePath -> Nbparts.UnembeddedMimeBundle -> IO Ipynb.MimeBundle
embedMimeBundle readDir = coerce (fmap Ipynb.MimeBundle . traverse (embedMimeData readDir))

embedMimeData :: FilePath -> Nbparts.UnembeddedMimeData -> IO Ipynb.MimeData
embedMimeData readDir (Nbparts.BinaryData path) = do
  bytes <- ByteString.readFile $ readDir </> path
  return $ Ipynb.BinaryData bytes
embedMimeData _ (Nbparts.TextualData text) = pure $ Ipynb.TextualData text
embedMimeData _ (Nbparts.JsonData value) = pure $ Ipynb.JsonData value
