module Nbparts.Unpack.Mime where

import Crypto.Hash qualified as Hash
import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64.URL
import Data.ByteString.Char8 qualified as ByteString
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
        (\attName bundle -> unembedMimeBundleWith (genFileName attName) dirPrefix subdir bundle)
  where
    -- Include the attachment name in the bytes before hashing so that
    -- different attachments with the same content have different hashes.
    -- This is needed so that we know which attachment to reference when
    -- repacking the notebook.
    genFileName :: Text -> Text -> ByteString -> FilePath
    genFileName attName mimeType bytes = binaryOutputFileName mimeType (Text.encodeUtf8 attName <> bytes)

unembedMimeBundleWith :: (Text -> ByteString -> FilePath) -> FilePath -> FilePath -> Ipynb.MimeBundle -> IO Nbparts.UnembeddedMimeBundle
unembedMimeBundleWith genFileName dirPrefix subdir =
  coerce $
    fmap Nbparts.UnembeddedMimeBundle
      . Map.traverseWithKey
        (unembedMimeDataWith genFileName dirPrefix subdir)

unembedMimeBundle :: FilePath -> FilePath -> Ipynb.MimeBundle -> IO Nbparts.UnembeddedMimeBundle
unembedMimeBundle = unembedMimeBundleWith binaryOutputFileName

unembedMimeDataWith ::
  (Text -> ByteString -> FilePath) ->
  FilePath ->
  FilePath ->
  Ipynb.MimeType ->
  Ipynb.MimeData ->
  IO Nbparts.UnembeddedMimeData
unembedMimeDataWith genFileName dirPrefix subdir mimetype (Ipynb.BinaryData bytes) = do
  let filename = genFileName mimetype bytes

  let relPath = subdir </> filename
  let writePath = dirPrefix </> relPath
  ByteString.writeFile writePath bytes
  return $ Nbparts.BinaryData relPath
unembedMimeDataWith _ _ _ _ (Ipynb.TextualData text) = pure $ Nbparts.TextualData text
unembedMimeDataWith _ _ _ _ (Ipynb.JsonData value) = pure $ Nbparts.JsonData value

unembedMimeData :: FilePath -> FilePath -> Ipynb.MimeType -> Ipynb.MimeData -> IO Nbparts.UnembeddedMimeData
unembedMimeData = unembedMimeDataWith binaryOutputFileName

binaryOutputFileName :: Text -> ByteString -> FilePath
binaryOutputFileName mimetype bytes =
  digestToBase64 (Hash.hashWith Hash.SHA256 bytes)
    <> case extensionFromMimeType mimetype of
      Nothing -> ""
      Just ext -> "." <> Text.unpack ext
  where
    digestToBase64 :: Hash.Digest a -> String
    digestToBase64 digest = ByteString.unpack . Base64.URL.encode $ ByteArray.convert digest

extensionFromMimeType :: Text -> Maybe Text
extensionFromMimeType "text/plain" = Just "txt"
extensionFromMimeType "image/tiff" = Just "tiff"
extensionFromMimeType "image/jpeg" = Just "jpg"
extensionFromMimeType mt = do
  exts <- Map.lookup (Text.encodeUtf8 mt) Mime.defaultExtensionMap
  Maybe.listToMaybe exts
