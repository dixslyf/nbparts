module Nbparts.Types.Sources
  ( CellSource (..),
    CellType (..),
    CellMarker (..),
    AttachmentNames (..),
    ExtractedAttachmentFilePath,
    AttachmentName,
  )
where

import Data.Aeson (Options (constructorTagModifier, sumEncoding))
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Nbparts.Types.Mime (UnembeddedMimeAttachments)

data CellSource = CellSource
  { id :: Text,
    cellType :: CellType,
    lines :: [Text],
    attachments :: Maybe UnembeddedMimeAttachments
  }
  deriving (Generic, Show)

data CellType = Markdown | Heading {headingLevel :: Int} | Raw | Code
  deriving (Generic, Show)

data CellMarker = CellMarker
  { cellId :: Text,
    cellType :: CellType,
    attachmentNames :: Maybe AttachmentNames
  }
  deriving (Generic, Show)

newtype AttachmentNames = AttachmentNames (Map ExtractedAttachmentFilePath AttachmentName)
  deriving (Generic, Show)

type ExtractedAttachmentFilePath = Text

type AttachmentName = Text

instance Aeson.ToJSON CellSource where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CellSource where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance Aeson.ToJSON CellType where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CellType where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance Aeson.ToJSON CellMarker

instance Aeson.FromJSON CellMarker

instance Aeson.ToJSON AttachmentNames

instance Aeson.FromJSON AttachmentNames

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
        other -> other
    }
