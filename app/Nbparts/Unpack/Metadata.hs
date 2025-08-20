module Nbparts.Unpack.Metadata where

import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Text (Text)
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error (UnpackError)
import Nbparts.Unpack.Error qualified as Nbparts

collectMetadata :: Ipynb.Notebook a -> Either UnpackError Nbparts.Metadata
collectMetadata (Ipynb.Notebook nbmeta (formatMajor, formatMinor) cells) = do
  cellsMetaList <- traverse extractCellMetadata cells
  let cellsMeta = Map.fromList cellsMetaList
  return Nbparts.Metadata {formatMajor, formatMinor, notebook = nbmeta, cells = cellsMeta}

extractCellMetadata :: Ipynb.Cell a -> Either UnpackError (Text, Nbparts.CellMetadata)
extractCellMetadata (Ipynb.Cell (Ipynb.Code exeCount _) maybeCellId _ meta _) = case maybeCellId of
  Just cellId -> Right (cellId, Nbparts.CodeCellMetadata exeCount meta)
  Nothing -> Left Nbparts.UnpackMissingCellIdError
extractCellMetadata (Ipynb.Cell _ maybeCellId _ meta _) = case maybeCellId of
  Just cellId -> Right (cellId, Nbparts.GenericCellMetadata meta)
  Nothing -> Left Nbparts.UnpackMissingCellIdError
