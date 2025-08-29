module Nbparts.Types.Sources
  ( CellSource (..),
    CellType (..),
    CellMarker (..),
  )
where

import Data.Aeson (Options (constructorTagModifier, sumEncoding))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Nbparts.Types.Mime (UnembeddedMimeAttachments)

data CellSource = CellSource
  { id :: Text,
    cellType :: CellType,
    lines :: [Text],
    attachments :: Maybe UnembeddedMimeAttachments
  }
  deriving (Generic, Show, Eq, Ord)

data CellType = Markdown | Raw | Code
  deriving (Generic, Show, Eq, Ord)

data CellMarker = CellMarker
  { id :: Text,
    cellType :: CellType,
    attachments :: Maybe UnembeddedMimeAttachments
  }
  deriving (Generic, Show, Eq, Ord)

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
        "Raw" -> "raw"
        "Code" -> "code"
        other -> other
    }
