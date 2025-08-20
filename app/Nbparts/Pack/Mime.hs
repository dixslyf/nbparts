module Nbparts.Pack.Mime where

import Data.ByteString qualified as ByteString
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Nbparts.Types qualified as Nbparts
import Network.Mime qualified as Mime
import System.FilePath ((</>))

adjustMimeAttachmentsPaths :: FilePath -> Nbparts.UnembeddedMimeAttachments -> Nbparts.UnembeddedMimeAttachments
adjustMimeAttachmentsPaths prefixDir (Nbparts.UnembeddedMimeAttachments attachments) =
  Nbparts.UnembeddedMimeAttachments $ Map.map (adjustMimeBundlePaths prefixDir) attachments

adjustMimeBundlePaths :: FilePath -> Nbparts.UnembeddedMimeBundle -> Nbparts.UnembeddedMimeBundle
adjustMimeBundlePaths prefixDir = Map.map (adjustMimeDataPaths prefixDir)

adjustMimeDataPaths :: FilePath -> Nbparts.UnembeddedMimeData -> Nbparts.UnembeddedMimeData
adjustMimeDataPaths prefixDir (Nbparts.BinaryData path) = Nbparts.BinaryData $ prefixDir </> path
adjustMimeDataPaths _ mimeData = mimeData

embedMimeAttachments :: Nbparts.UnembeddedMimeAttachments -> IO Ipynb.MimeAttachments
embedMimeAttachments (Nbparts.UnembeddedMimeAttachments unembedded) = Ipynb.MimeAttachments <$> traverse embedMimeBundle unembedded

embedMimeBundle :: Nbparts.UnembeddedMimeBundle -> IO Ipynb.MimeBundle
embedMimeBundle unembeddedMimeBundle = Ipynb.MimeBundle <$> traverse embedMimeData unembeddedMimeBundle

embedMimeData :: Nbparts.UnembeddedMimeData -> IO Ipynb.MimeData
embedMimeData (Nbparts.BinaryData path) = do
  bytes <- ByteString.readFile path
  return $ Ipynb.BinaryData bytes
embedMimeData (Nbparts.TextualData text) = pure $ Ipynb.TextualData text
embedMimeData (Nbparts.JsonData value) = pure $ Ipynb.JsonData value

-- TODO: Should the mime bundle really be a singleton? Is it possible for there to be multiple entries?
mediaPathToMimeBundle :: Text -> Nbparts.UnembeddedMimeBundle
mediaPathToMimeBundle fp =
  Map.singleton
    (Text.decodeUtf8 $ Mime.defaultMimeLookup fp)
    $ Nbparts.BinaryData (Text.unpack fp)
