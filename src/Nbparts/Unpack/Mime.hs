module Nbparts.Unpack.Mime where

import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as State
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
import Nbparts.Types
  ( UnembeddedMimeAttachments (UnembeddedMimeAttachments),
    UnembeddedMimeBundle (UnembeddedMimeBundle),
    UnembeddedMimeData (BinaryData, JsonData, TextualData),
  )
import Network.Mime qualified as Mime
import System.FilePath ((</>))

unembedMimeAttachments :: (MonadState [(FilePath, ByteString)] m) => FilePath -> Ipynb.MimeAttachments -> m UnembeddedMimeAttachments
unembedMimeAttachments subdir mimeAtts = UnembeddedMimeAttachments <$> Map.traverseWithKey go (coerce mimeAtts)
  where
    go :: (MonadState [(FilePath, ByteString)] m) => Text -> Ipynb.MimeBundle -> m UnembeddedMimeBundle
    go attName = unembedMimeBundleWith (genFileName attName) subdir

    -- Include the attachment name in the bytes before hashing so that
    -- different attachments with the same content have different hashes.
    -- This is needed so that we know which attachment to reference when
    -- repacking the notebook.
    genFileName :: Text -> Text -> ByteString -> FilePath
    genFileName attName mimeType bytes = binaryOutputFileName mimeType (Text.encodeUtf8 attName <> bytes)

unembedMimeBundleWith ::
  (MonadState [(FilePath, ByteString)] m) =>
  (Text -> ByteString -> FilePath) ->
  FilePath ->
  Ipynb.MimeBundle ->
  m UnembeddedMimeBundle
unembedMimeBundleWith genFileName subdir mimeBundle = UnembeddedMimeBundle <$> Map.traverseWithKey go (coerce mimeBundle)
  where
    go :: (MonadState [(FilePath, ByteString)] m) => Ipynb.MimeType -> Ipynb.MimeData -> m UnembeddedMimeData
    go mimeType mimeData = do
      let (uMimeData, maybeExport) = unembedMimeDataWith genFileName subdir mimeType mimeData
      State.modify (Maybe.maybe id (:) maybeExport)
      pure uMimeData

unembedMimeBundle :: (MonadState [(FilePath, ByteString)] m) => FilePath -> Ipynb.MimeBundle -> m UnembeddedMimeBundle
unembedMimeBundle = unembedMimeBundleWith binaryOutputFileName

unembedMimeDataWith ::
  (Text -> ByteString -> FilePath) ->
  FilePath ->
  Ipynb.MimeType ->
  Ipynb.MimeData ->
  (UnembeddedMimeData, Maybe (FilePath, ByteString))
unembedMimeDataWith genFileName subdir mimetype (Ipynb.BinaryData bytes) =
  let filename = genFileName mimetype bytes
      filepath = subdir </> filename
   in (BinaryData filepath, Just (filepath, bytes))
unembedMimeDataWith _ _ _ (Ipynb.TextualData text) = (TextualData text, Nothing)
unembedMimeDataWith _ _ _ (Ipynb.JsonData value) = (JsonData value, Nothing)

unembedMimeData ::
  FilePath ->
  Ipynb.MimeType ->
  Ipynb.MimeData ->
  (UnembeddedMimeData, Maybe (FilePath, ByteString))
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
