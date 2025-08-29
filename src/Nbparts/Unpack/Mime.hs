module Nbparts.Unpack.Mime where

import Crypto.Hash qualified as Hash
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Nbparts.Types qualified as Nbparts
import Network.Mime qualified as Mime
import System.FilePath ((</>))

unembedMimeAttachments :: FilePath -> FilePath -> Ipynb.MimeAttachments -> IO Nbparts.UnembeddedMimeAttachments
unembedMimeAttachments dirPrefix subdir =
  coerce $
    fmap Nbparts.UnembeddedMimeAttachments
      . Map.traverseWithKey
        (\attName bundle -> unembedMimeBundleWithPrefix (Text.unpack attName <> "-") dirPrefix subdir bundle)

unembedMimeBundleWithPrefix :: String -> FilePath -> FilePath -> Ipynb.MimeBundle -> IO Nbparts.UnembeddedMimeBundle
unembedMimeBundleWithPrefix fpPrefix dirPrefix subdir =
  coerce $
    fmap Nbparts.UnembeddedMimeBundle
      . Map.traverseWithKey
        (unembedMimeDataWithPrefix fpPrefix dirPrefix subdir)

unembedMimeBundle :: FilePath -> FilePath -> Ipynb.MimeBundle -> IO Nbparts.UnembeddedMimeBundle
unembedMimeBundle = unembedMimeBundleWithPrefix ""

unembedMimeDataWithPrefix :: String -> FilePath -> FilePath -> Ipynb.MimeType -> Ipynb.MimeData -> IO Nbparts.UnembeddedMimeData
unembedMimeDataWithPrefix fpPrefix dirPrefix subdir mimetype (Ipynb.BinaryData bytes) = do
  let filename = fpPrefix <> binaryOutputFileName mimetype bytes
  let relPath = subdir </> filename
  let writePath = dirPrefix </> relPath
  ByteString.writeFile writePath bytes
  return $ Nbparts.BinaryData relPath
unembedMimeDataWithPrefix _ _ _ _ (Ipynb.TextualData text) = pure $ Nbparts.TextualData text
unembedMimeDataWithPrefix _ _ _ _ (Ipynb.JsonData value) = pure $ Nbparts.JsonData value

unembedMimeData :: FilePath -> FilePath -> Ipynb.MimeType -> Ipynb.MimeData -> IO Nbparts.UnembeddedMimeData
unembedMimeData = unembedMimeDataWithPrefix ""

binaryOutputFileName :: Text -> ByteString -> FilePath
binaryOutputFileName mimetype bytes =
  show (Hash.hashWith Hash.SHA1 bytes)
    <> case extensionFromMimeType mimetype of
      Nothing -> ""
      Just ext -> "." <> Text.unpack ext

extensionFromMimeType :: Text -> Maybe Text
extensionFromMimeType "text/plain" = Just "txt"
extensionFromMimeType "image/tiff" = Just "tiff"
extensionFromMimeType "image/jpeg" = Just "jpg"
extensionFromMimeType mt = do
  exts <- Map.lookup (Text.encodeUtf8 mt) Mime.defaultExtensionMap
  Maybe.listToMaybe exts
