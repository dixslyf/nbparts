module Nbparts.Types.Metadata
  ( NotebookMetadata (..),
    CellMetadata (..),
  )
where

import Data.Aeson (Options (constructorTagModifier, sumEncoding))
import Data.Aeson qualified as Aeson
import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data NotebookMetadata = NotebookMetadata
  { formatMajor :: Int,
    formatMinor :: Int,
    toplevel :: Ipynb.JSONMeta,
    cells :: Map Text CellMetadata -- Map of Cell IDs to attributes.
  }
  deriving (Generic, Show)

data CellMetadata
  = CodeCellMetadata {executionCount :: Maybe Int, genericMetadata :: Ipynb.JSONMeta}
  | GenericCellMetadata Ipynb.JSONMeta
  deriving (Generic, Show)

instance Aeson.ToJSON NotebookMetadata

instance Aeson.FromJSON NotebookMetadata

instance Aeson.ToJSON CellMetadata where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CellMetadata where
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
        "CodeCellMetadata" -> "codeMetadata"
        "GenericCellMetadata" -> "genericMetadata"
        other -> other
    }
