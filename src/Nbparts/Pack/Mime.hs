module Nbparts.Pack.Mime where

import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Nbparts.Types qualified as Nbparts
import System.FilePath ((</>))

adjustMimeAttachmentsPaths :: FilePath -> Nbparts.UnembeddedMimeAttachments -> Nbparts.UnembeddedMimeAttachments
adjustMimeAttachmentsPaths prefixDir (Nbparts.UnembeddedMimeAttachments attachments) =
  Nbparts.UnembeddedMimeAttachments $ Map.map (adjustMimeBundlePaths prefixDir) attachments

adjustMimeBundlePaths :: FilePath -> Nbparts.UnembeddedMimeBundle -> Nbparts.UnembeddedMimeBundle
adjustMimeBundlePaths prefixDir = coerce $ Map.map (adjustMimeDataPaths prefixDir)

adjustMimeDataPaths :: FilePath -> Nbparts.UnembeddedMimeData -> Nbparts.UnembeddedMimeData
adjustMimeDataPaths prefixDir (Nbparts.BinaryData path) = Nbparts.BinaryData $ prefixDir </> path
adjustMimeDataPaths _ mimeData = mimeData

embedMimeAttachments :: Nbparts.UnembeddedMimeAttachments -> IO Ipynb.MimeAttachments
embedMimeAttachments (Nbparts.UnembeddedMimeAttachments unembedded) = Ipynb.MimeAttachments <$> traverse embedMimeBundle unembedded

embedMimeBundle :: Nbparts.UnembeddedMimeBundle -> IO Ipynb.MimeBundle
embedMimeBundle = coerce (fmap Ipynb.MimeBundle . traverse embedMimeData)

embedMimeData :: Nbparts.UnembeddedMimeData -> IO Ipynb.MimeData
embedMimeData (Nbparts.BinaryData path) = do
  bytes <- ByteString.readFile path
  return $ Ipynb.BinaryData bytes
embedMimeData (Nbparts.TextualData text) = pure $ Ipynb.TextualData text
embedMimeData (Nbparts.JsonData value) = pure $ Ipynb.JsonData value
