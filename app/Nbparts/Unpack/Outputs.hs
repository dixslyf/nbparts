{-# LANGUAGE LambdaCase #-}

module Nbparts.Unpack.Outputs where

import Crypto.Hash qualified as Hash
import Data.Aeson (Options (..), SumEncoding (..))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Nbparts.Unpack.Error (UnpackError)
import Nbparts.Unpack.Error qualified as Nbparts
import System.FilePath ((</>))
import Text.Pandoc.MIME qualified as Pandoc.MIME

type Outputs a = Map Text [Ipynb.Output a] -- Map of Cell IDs to outputs.

type UnembeddedOutputs = Map Text [UnembeddedOutput]

data UnembeddedMimeData = BinaryData FilePath | TextualData Text | JsonData Aeson.Value
  deriving (Generic, Show)

instance Aeson.ToJSON UnembeddedMimeData where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON UnembeddedMimeData where
  parseJSON = Aeson.genericParseJSON jsonOptions

type UnembeddedMimeBundle = Map Text UnembeddedMimeData

data UnembeddedOutput
  = Stream
      { streamName :: Text,
        streamText :: [Text]
      }
  | DisplayData
      { displayData :: UnembeddedMimeBundle,
        displayMetadata :: Ipynb.JSONMeta
      }
  | ExecuteResult
      { executeCount :: Int,
        executeData :: UnembeddedMimeBundle,
        executeMetadata :: Ipynb.JSONMeta
      }
  | Err
      { errName :: Text,
        errValue :: Text,
        errTraceback :: [Text]
      }
  deriving (Generic, Show)

instance Aeson.ToJSON UnembeddedOutput where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON UnembeddedOutput where
  parseJSON = Aeson.genericParseJSON jsonOptions

jsonOptions :: Aeson.Options
jsonOptions =
  Aeson.defaultOptions
    { sumEncoding =
        Aeson.TaggedObject
          { tagFieldName = "type",
            contentsFieldName = "value"
          },
      constructorTagModifier = \case
        "BinaryData" -> "binary"
        "TextualData" -> "text"
        "JsonData" -> "json"
        "Stream" -> "stream"
        "DisplayData" -> "display-data"
        "ExecuteResult" -> "execute-result"
        "Err" -> "error"
        other -> other
    }

collectOutputs :: Ipynb.Notebook a -> Either UnpackError (Outputs a)
collectOutputs (Ipynb.Notebook _meta _format cells) = Map.fromList <$> sequence (Maybe.mapMaybe toEntry cells)
  where
    toEntry :: Ipynb.Cell a -> Maybe (Either UnpackError (Text, [Ipynb.Output a]))
    toEntry (Ipynb.Cell (Ipynb.Code _exeCount outputs) maybeCellId _ _ _) = Just $ case maybeCellId of
      Just cellId -> Right (cellId, outputs)
      Nothing -> Left Nbparts.UnpackMissingCellIdError
    toEntry _ = Nothing

unembedOutputs :: FilePath -> FilePath -> Outputs a -> IO UnembeddedOutputs
unembedOutputs dirPrefix subdir = traverse $ mapM (unembedOutput dirPrefix subdir)

unembedOutput :: FilePath -> FilePath -> Ipynb.Output a -> IO UnembeddedOutput
unembedOutput _ _ (Ipynb.Stream streamName (Ipynb.Source streamText)) = pure $ Stream {streamName, streamText}
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

unembedMimeBundle :: FilePath -> FilePath -> Ipynb.MimeBundle -> IO UnembeddedMimeBundle
unembedMimeBundle dirPrefix subdir (Ipynb.MimeBundle mimeBundle) = Map.traverseWithKey (unembedMimeData dirPrefix subdir) mimeBundle

unembedMimeData :: FilePath -> FilePath -> Ipynb.MimeType -> Ipynb.MimeData -> IO UnembeddedMimeData
unembedMimeData dirPrefix subdir mimetype (Ipynb.BinaryData bytes) = do
  let filename = binaryOutputFileName mimetype bytes
  let relPath = subdir </> filename
  let writePath = dirPrefix </> relPath
  ByteString.writeFile writePath bytes
  return $ BinaryData relPath
unembedMimeData _ _ _ (Ipynb.TextualData text) = pure $ TextualData text
unembedMimeData _ _ _ (Ipynb.JsonData value) = pure $ JsonData value

binaryOutputFileName :: Text -> ByteString -> FilePath
binaryOutputFileName mimetype bytes =
  show (Hash.hashWith Hash.SHA1 bytes)
    <> case Pandoc.MIME.extensionFromMimeType mimetype of
      Nothing -> ""
      Just ext -> "." <> Text.unpack ext
