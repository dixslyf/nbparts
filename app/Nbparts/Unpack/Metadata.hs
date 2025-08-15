module Nbparts.Unpack.Metadata where

import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Text (Text)
import Nbparts.Types (Metadata (..))
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error (UnpackError)
import Nbparts.Unpack.Error qualified as Nbparts

collectMetadata :: Ipynb.Notebook a -> Either UnpackError Nbparts.Metadata
collectMetadata (Ipynb.Notebook nbmeta _format cells) = do
  cellsMetaList <- traverse extractCellMetadata cells
  let cellsMeta = Map.fromList cellsMetaList
  return Nbparts.Metadata {notebook = nbmeta, cells = cellsMeta}

extractCellMetadata :: Ipynb.Cell a -> Either UnpackError (Text, Ipynb.JSONMeta)
extractCellMetadata (Ipynb.Cell _ maybeCellId _ meta _) = case maybeCellId of
  Just cellId -> Right (cellId, meta)
  Nothing -> Left Nbparts.UnpackMissingCellIdError
