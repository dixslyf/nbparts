module Nbparts.Types where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson (Options (constructorTagModifier, sumEncoding))
import Data.Aeson qualified as Aeson
import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version qualified
import GHC.Generics (Generic)
import Paths_nbparts qualified

currentNbpartsVersion :: Text
currentNbpartsVersion = Text.pack $ Data.Version.showVersion Paths_nbparts.version

data NbpartsManifest = NbpartsManifest
  { nbpartsVersion :: Text,
    sourcesFormat :: NbpartsFormat
  }
  deriving (Generic, Show)

mkNbpartsManifest :: NbpartsFormat -> NbpartsManifest
mkNbpartsManifest = NbpartsManifest currentNbpartsVersion

instance Aeson.ToJSON NbpartsManifest where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON NbpartsManifest where
  parseJSON = Aeson.genericParseJSON jsonOptions

data NbpartsFormat = FormatYaml | FormatMarkdown
  deriving (Generic, Show)

instance Aeson.ToJSON NbpartsFormat where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON NbpartsFormat where
  parseJSON = Aeson.genericParseJSON jsonOptions

data SomeNotebook where
  SomeNotebook :: (Aeson.ToJSON (Ipynb.Notebook a), Aeson.FromJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> SomeNotebook

instance Show SomeNotebook where
  show (SomeNotebook nb) = "SomeNotebook (" <> show nb <> ")"

instance Aeson.ToJSON SomeNotebook where
  toJSON (SomeNotebook nb) = Aeson.toJSON nb

instance Aeson.FromJSON SomeNotebook where
  parseJSON v =
    SomeNotebook <$> (Aeson.parseJSON @(Ipynb.Notebook Ipynb.NbV3)) v
      <|> SomeNotebook <$> (Aeson.parseJSON @(Ipynb.Notebook Ipynb.NbV4)) v

withSomeNotebook :: SomeNotebook -> (forall a. (Aeson.ToJSON (Ipynb.Notebook a), Aeson.FromJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> r) -> r
withSomeNotebook (SomeNotebook nb) f = f nb

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
        "FormatYaml" -> "yaml"
        "FormatMarkdown" -> "markdown"
        other -> other
    }

data CellInfo = CellInfo
  { cellId :: Text,
    cellType :: CellType,
    attachmentUrls :: Maybe CellAttachmentUrls
  }
  deriving (Generic, Show)

instance Aeson.ToJSON CellInfo

instance Aeson.FromJSON CellInfo

newtype CellAttachmentUrls = CellAttachmentUrls (Map Text Text)
  deriving (Generic, Show)

instance Aeson.ToJSON CellAttachmentUrls

instance Aeson.FromJSON CellAttachmentUrls
