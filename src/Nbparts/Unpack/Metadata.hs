module Nbparts.Unpack.Metadata where

import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Nbparts.Types qualified as Nbparts

extractNotebookVersion :: Ipynb.Notebook a -> (Int, Int)
extractNotebookVersion (Ipynb.Notebook _ format _) = format

collectMetadata :: Ipynb.Notebook a -> Either Nbparts.UnpackError Nbparts.NotebookMetadata
collectMetadata (Ipynb.Notebook toplevelMeta (formatMajor, formatMinor) cells) = do
  cellsMetaList <- traverse extractCellMetadata cells
  -- Remove empty metadata for compactness.
  let filtered = filter (not . isEmptyMeta . snd) cellsMetaList
  let cellsMeta = Map.fromList filtered
  return Nbparts.NotebookMetadata {formatMajor, formatMinor, toplevel = toplevelMeta, cells = cellsMeta}

isEmptyMeta :: Nbparts.CellMetadata -> Bool
isEmptyMeta (Nbparts.CodeCellMetadata exeCount (Ipynb.JSONMeta meta)) = Maybe.isNothing exeCount && Map.null meta
isEmptyMeta (Nbparts.GenericCellMetadata (Ipynb.JSONMeta meta)) = Map.null meta

extractCellMetadata :: Ipynb.Cell a -> Either Nbparts.UnpackError (Text, Nbparts.CellMetadata)
extractCellMetadata (Ipynb.Cell (Ipynb.Code exeCount _) maybeCellId _ meta _) = case maybeCellId of
  Just cellId -> Right (cellId, Nbparts.CodeCellMetadata exeCount meta)
  Nothing -> Left Nbparts.UnpackMissingCellIdError
extractCellMetadata (Ipynb.Cell _ maybeCellId _ meta _) = case maybeCellId of
  Just cellId -> Right (cellId, Nbparts.GenericCellMetadata meta)
  Nothing -> Left Nbparts.UnpackMissingCellIdError
