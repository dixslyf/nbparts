module Nbparts.Unpack.Metadata where

import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Nbparts.Types
  ( CellMetadata (CodeCellMetadata, GenericCellMetadata),
    NotebookMetadata (NotebookMetadata, cells, formatMajor, formatMinor, toplevel),
    UnpackError (UnpackMissingCellIdError),
  )

extractNotebookVersion :: Ipynb.Notebook a -> (Int, Int)
extractNotebookVersion (Ipynb.Notebook _ format _) = format

collectMetadata :: Ipynb.Notebook a -> Either UnpackError NotebookMetadata
collectMetadata (Ipynb.Notebook toplevelMeta (formatMajor, formatMinor) cells) = do
  cellsMetaList <- traverse extractCellMetadata cells
  -- Remove empty metadata for compactness.
  let filtered = filter (not . isEmptyMeta . snd) cellsMetaList
  let cellsMeta = Map.fromList filtered
  return NotebookMetadata {formatMajor, formatMinor, toplevel = toplevelMeta, cells = cellsMeta}

isEmptyMeta :: CellMetadata -> Bool
isEmptyMeta (CodeCellMetadata exeCount (Ipynb.JSONMeta meta)) = Maybe.isNothing exeCount && Map.null meta
isEmptyMeta (GenericCellMetadata (Ipynb.JSONMeta meta)) = Map.null meta

extractCellMetadata :: Ipynb.Cell a -> Either UnpackError (Text, CellMetadata)
extractCellMetadata (Ipynb.Cell (Ipynb.Code exeCount _) maybeCellId _ meta _) = case maybeCellId of
  Just cellId -> Right (cellId, CodeCellMetadata exeCount meta)
  Nothing -> Left UnpackMissingCellIdError
extractCellMetadata (Ipynb.Cell _ maybeCellId _ meta _) = case maybeCellId of
  Just cellId -> Right (cellId, GenericCellMetadata meta)
  Nothing -> Left UnpackMissingCellIdError
