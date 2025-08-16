{-# LANGUAGE LambdaCase #-}

module Nbparts.Types where

import Data.Aeson (Options (..), SumEncoding (..))
import Data.Aeson qualified as Aeson
import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data Source = Source
  { cellType :: CellType,
    cellId :: Text,
    source :: [Text],
    attachments :: Maybe UnembeddedMimeAttachments
  }
  deriving (Generic, Show)

instance Aeson.ToJSON Source where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON Source where
  parseJSON = Aeson.genericParseJSON jsonOptions

data CellType = Markdown | Heading {headingLevel :: Int} | Raw | Code
  deriving (Generic, Show)

instance Aeson.ToJSON CellType where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CellType where
  parseJSON = Aeson.genericParseJSON jsonOptions

newtype UnembeddedMimeAttachments = UnembeddedMimeAttachments (Map Text UnembeddedMimeBundle)
  deriving (Generic, Show)

instance Aeson.ToJSON UnembeddedMimeAttachments

instance Aeson.FromJSON UnembeddedMimeAttachments

data Metadata = Metadata
  { formatMajor :: Int,
    formatMinor :: Int,
    notebook :: Ipynb.JSONMeta,
    cells :: Map Text CellMetadata -- Map of Cell IDs to attributes.
  }
  deriving (Generic, Show)

instance Aeson.ToJSON Metadata

instance Aeson.FromJSON Metadata

data CellMetadata
  = CodeCellMetadata {executionCount :: Maybe Int, cellMetadata :: Ipynb.JSONMeta}
  | GenericCellMetadata Ipynb.JSONMeta
  deriving (Generic, Show)

-- For error reporting.
data CellMetadataTag = CodeCellMetadataTag | GenericCellMetadataTag

instance Aeson.ToJSON CellMetadata where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CellMetadata where
  parseJSON = Aeson.genericParseJSON jsonOptions

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
        "Markdown" -> "markdown"
        "Heading" -> "heading"
        "Raw" -> "raw"
        "Code" -> "code"
        "BinaryData" -> "binary"
        "TextualData" -> "text"
        "JsonData" -> "json"
        "Stream" -> "stream"
        "DisplayData" -> "display-data"
        "ExecuteResult" -> "execute-result"
        "Err" -> "error"
        "CodeCellMetadata" -> "code-cell"
        "GenericCellMetadata" -> "generic-cell"
        other -> other
    }
