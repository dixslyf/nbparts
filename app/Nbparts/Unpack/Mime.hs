module Nbparts.Unpack.Mime where

import Crypto.Hash qualified as Hash
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Types qualified as Nbparts
import System.FilePath ((</>))
import Text.Pandoc.MIME qualified as Pandoc.MIME

unembedMimeAttachments :: FilePath -> FilePath -> Ipynb.MimeAttachments -> IO Nbparts.UnembeddedMimeAttachments
unembedMimeAttachments dirPrefix subdir (Ipynb.MimeAttachments mimeAttachments) =
  Nbparts.UnembeddedMimeAttachments <$> traverse (unembedMimeBundle dirPrefix subdir) mimeAttachments

unembedMimeBundle :: FilePath -> FilePath -> Ipynb.MimeBundle -> IO Nbparts.UnembeddedMimeBundle
unembedMimeBundle dirPrefix subdir (Ipynb.MimeBundle mimeBundle) = Map.traverseWithKey (unembedMimeData dirPrefix subdir) mimeBundle

unembedMimeData :: FilePath -> FilePath -> Ipynb.MimeType -> Ipynb.MimeData -> IO Nbparts.UnembeddedMimeData
unembedMimeData dirPrefix subdir mimetype (Ipynb.BinaryData bytes) = do
  let filename = binaryOutputFileName mimetype bytes
  let relPath = subdir </> filename
  let writePath = dirPrefix </> relPath
  ByteString.writeFile writePath bytes
  return $ Nbparts.BinaryData relPath
unembedMimeData _ _ _ (Ipynb.TextualData text) = pure $ Nbparts.TextualData text
unembedMimeData _ _ _ (Ipynb.JsonData value) = pure $ Nbparts.JsonData value

binaryOutputFileName :: Text -> ByteString -> FilePath
binaryOutputFileName mimetype bytes =
  show (Hash.hashWith Hash.SHA1 bytes)
    <> case Pandoc.MIME.extensionFromMimeType mimetype of
      Nothing -> ""
      Just ext -> "." <> Text.unpack ext
