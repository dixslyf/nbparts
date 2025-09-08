module Nbparts.Pack.Metadata where

import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Nbparts.Types
  ( CellMetadata (CodeCellMetadata, GenericCellMetadata),
    CellMetadataTag (CodeCellMetadataTag, GenericCellMetadataTag),
    NotebookMetadata (NotebookMetadata),
    PackError (PackCellMetadataTypeMismatch, PackMissingCellIdError, actual, expected),
    emptyCodeMetadata,
    emptyGenericMetadata,
  )

fillMetadata :: NotebookMetadata -> Ipynb.Notebook a -> Either PackError (Ipynb.Notebook a)
fillMetadata (NotebookMetadata formatMajor formatMinor nbMeta cellsMeta) (Ipynb.Notebook _ _ cells) =
  Ipynb.Notebook
    nbMeta
    (formatMajor, formatMinor)
    <$> filledCells
  where
    filledCells = traverse (fillCellMetadata cellsMeta) cells

fillCellMetadata :: Map Text CellMetadata -> Ipynb.Cell a -> Either PackError (Ipynb.Cell a)
fillCellMetadata metaMap (Ipynb.Cell cellType maybeCellId source _ attachments) = do
  cellId <- case maybeCellId of
    Just cId -> Right cId
    Nothing -> Left PackMissingCellIdError
  let cellMeta = case (Map.lookup cellId metaMap, cellType) of
        (Just meta, _) -> meta
        -- If we can't find the metadata, assume there is none.
        (Nothing, Ipynb.Code _ _) -> emptyCodeMetadata
        (Nothing, _) -> emptyGenericMetadata
  case (cellType, cellMeta) of
    -- Code cell expects CodeCellMetadata
    (Ipynb.Code _ outputs, CodeCellMetadata exeCount meta) ->
      Right $ Ipynb.Cell (Ipynb.Code exeCount outputs) (Just cellId) source meta attachments
    (Ipynb.Code _ _, GenericCellMetadata _) ->
      Left $
        PackCellMetadataTypeMismatch
          { expected = CodeCellMetadataTag,
            actual = GenericCellMetadataTag
          }
    -- Other cell types expect GenericCellMetadata
    (_, GenericCellMetadata meta) ->
      Right $ Ipynb.Cell cellType (Just cellId) source meta attachments
    (_, CodeCellMetadata _ _) ->
      Left $
        PackCellMetadataTypeMismatch
          { expected = GenericCellMetadataTag,
            actual = CodeCellMetadataTag
          }
