module Nbparts.Pack.Metadata where

import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Nbparts.Pack.Error qualified as Nbparts
import Nbparts.Types qualified as Nbparts

fillMetadata :: Ipynb.Notebook a -> Nbparts.Metadata -> Either Nbparts.PackError (Ipynb.Notebook a)
fillMetadata (Ipynb.Notebook _ format cells) (Nbparts.Metadata nbMeta cellsMeta) = Ipynb.Notebook nbMeta format <$> filledCells
  where
    filledCells = traverse (fillCellMetadata cellsMeta) cells

fillCellMetadata :: Map Text Ipynb.JSONMeta -> Ipynb.Cell a -> Either Nbparts.PackError (Ipynb.Cell a)
fillCellMetadata cellsMeta (Ipynb.Cell cellType maybeCellId source _ attachments) = do
  cellId <- maybe (Left Nbparts.PackMissingCellIdError) Right maybeCellId
  cellMeta <- maybe (Left $ Nbparts.PackMissingCellMetadataError cellId) Right (Map.lookup cellId cellsMeta)
  pure $ Ipynb.Cell cellType (Just cellId) source cellMeta attachments
