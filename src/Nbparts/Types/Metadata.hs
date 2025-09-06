module Nbparts.Types.Metadata
  ( NotebookMetadata (..),
    CellMetadata (..),
    emptyCodeMetadata,
    emptyGenericMetadata,
  )
where

import Data.Aeson (Options (constructorTagModifier, sumEncoding))
import Data.Aeson qualified as Aeson
import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

data NotebookMetadata = NotebookMetadata
  { formatMajor :: Int,
    formatMinor :: Int,
    toplevel :: Ipynb.JSONMeta,
    cells :: Map Text CellMetadata -- Map of Cell IDs to attributes.
  }
  deriving (Generic, Show, Eq, Ord)

data CellMetadata
  = CodeCellMetadata {executionCount :: Maybe Int, meta :: Ipynb.JSONMeta}
  | GenericCellMetadata Ipynb.JSONMeta
  deriving (Generic, Show, Eq, Ord)

emptyCodeMetadata :: CellMetadata
emptyCodeMetadata = CodeCellMetadata Nothing $ Ipynb.JSONMeta Map.empty

emptyGenericMetadata :: CellMetadata
emptyGenericMetadata = GenericCellMetadata $ Ipynb.JSONMeta Map.empty

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
            contentsFieldName = "meta"
          },
      constructorTagModifier = \case
        "CodeCellMetadata" -> "code"
        "GenericCellMetadata" -> "generic"
        other -> other
    }
