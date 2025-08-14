module Nbparts.Unpack.Metadata where

import Data.Aeson qualified as Aeson
import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Nbparts.Unpack.Error (UnpackError)
import Nbparts.Unpack.Error qualified as Nbparts

data Metadata = Metadata
  { notebook :: Ipynb.JSONMeta,
    cells :: Map Text Ipynb.JSONMeta -- Map of Cell IDs to key-value attribute pairs.
  }
  deriving (Generic, Show)

instance Aeson.ToJSON Metadata

instance Aeson.FromJSON Metadata

collectMetadata :: Ipynb.Notebook a -> Either UnpackError Metadata
collectMetadata (Ipynb.Notebook nbmeta _format cells) = do
  cellsMetaList <- traverse extractCellMetadata cells
  let cellsMeta = Map.fromList cellsMetaList
  return Metadata {notebook = nbmeta, cells = cellsMeta}

extractCellMetadata :: Ipynb.Cell a -> Either UnpackError (Text, Ipynb.JSONMeta)
extractCellMetadata (Ipynb.Cell _ maybeCellId _ meta _) = case maybeCellId of
  Just cellId -> Right (cellId, meta)
  Nothing -> Left Nbparts.UnpackMissingCellIdError
