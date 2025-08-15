module Nbparts.Unpack.Outputs where

import Crypto.Hash qualified as Hash
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Types (UnembeddedOutput (..))
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error (UnpackError)
import Nbparts.Unpack.Error qualified as Nbparts
import System.FilePath ((</>))
import Text.Pandoc.MIME qualified as Pandoc.MIME

collectOutputs :: Ipynb.Notebook a -> Either UnpackError (Nbparts.Outputs a)
collectOutputs (Ipynb.Notebook _meta _format cells) = Map.fromList <$> sequence (Maybe.mapMaybe toEntry cells)
  where
    toEntry :: Ipynb.Cell a -> Maybe (Either UnpackError (Text, [Ipynb.Output a]))
    toEntry (Ipynb.Cell (Ipynb.Code _exeCount outputs) maybeCellId _ _ _) = Just $ case maybeCellId of
      Just cellId -> Right (cellId, outputs)
      Nothing -> Left Nbparts.UnpackMissingCellIdError
    toEntry _ = Nothing

unembedOutputs :: FilePath -> FilePath -> Nbparts.Outputs a -> IO Nbparts.UnembeddedOutputs
unembedOutputs dirPrefix subdir = traverse $ mapM (unembedOutput dirPrefix subdir)

unembedOutput :: FilePath -> FilePath -> Ipynb.Output a -> IO Nbparts.UnembeddedOutput
unembedOutput _ _ (Ipynb.Stream streamName (Ipynb.Source streamText)) = pure $ Nbparts.Stream {streamName, streamText}
unembedOutput dirPrefix subdir (Ipynb.DisplayData displayData displayMetadata) = do
  unembededDisplayData <- unembedMimeBundle dirPrefix subdir displayData
  return
    DisplayData
      { displayData = unembededDisplayData,
        displayMetadata
      }
unembedOutput dirPrefix subdir (Ipynb.ExecuteResult executeCount executeData executeMetadata) = do
  unembededExecuteData <- unembedMimeBundle dirPrefix subdir executeData
  return
    ExecuteResult
      { executeCount,
        executeData = unembededExecuteData,
        executeMetadata
      }
unembedOutput _ _ (Ipynb.Err errName errValue errTraceback) = pure $ Err {errName, errValue, errTraceback}

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
