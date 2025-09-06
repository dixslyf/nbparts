module Nbparts.Pack.Outputs where

import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Nbparts.Pack.Mime qualified as Nbparts
import Nbparts.Types qualified as Nbparts

fillOutputs ::
  (MonadError Nbparts.PackError m, MonadIO m) =>
  FilePath ->
  Nbparts.UnembeddedNotebookOutputs ->
  Ipynb.Notebook a ->
  m (Ipynb.Notebook a)
fillOutputs readDir unembeddedOutputs (Ipynb.Notebook meta format cells) = do
  outputs <- liftIO $ embedOutputs readDir unembeddedOutputs
  filledCells <- liftEither $ traverse (fillCellOutputs outputs) cells
  return $ Ipynb.Notebook meta format filledCells

fillCellOutputs :: Nbparts.NotebookOutputs a -> Ipynb.Cell a -> Either Nbparts.PackError (Ipynb.Cell a)
fillCellOutputs (Nbparts.NotebookOutputs outputs) (Ipynb.Cell (Ipynb.Code codeExecutionCount _) maybeCellId source meta attachments) = do
  cellId <- case maybeCellId of
    Just cellId -> Right cellId
    Nothing -> Left Nbparts.PackMissingCellIdError

  -- If we can't find the outputs, assume that there are no outputs.
  let cellOutputs = Maybe.fromMaybe [] $ Map.lookup cellId outputs

  return $ Ipynb.Cell (Ipynb.Code codeExecutionCount cellOutputs) (Just cellId) source meta attachments
fillCellOutputs _ cell = pure cell

embedOutputs :: FilePath -> Nbparts.UnembeddedNotebookOutputs -> IO (Nbparts.NotebookOutputs a)
embedOutputs readDir = coerce $ fmap Nbparts.NotebookOutputs . traverse (mapM $ embedOutput readDir)

embedOutput :: FilePath -> Nbparts.UnembeddedCellOutput -> IO (Ipynb.Output a)
embedOutput _ (Nbparts.Stream streamName streamText) = pure $ Ipynb.Stream {streamName, streamText = Ipynb.Source streamText}
embedOutput readDir (Nbparts.DisplayData unembeddedDisplayData displayMetadata) = do
  displayData <- Nbparts.embedMimeBundle readDir unembeddedDisplayData
  return
    Ipynb.DisplayData
      { displayData = displayData,
        displayMetadata
      }
embedOutput readDir (Nbparts.ExecuteResult executeCount unembeddedExecuteData executeMetadata) = do
  executeData <- Nbparts.embedMimeBundle readDir unembeddedExecuteData
  return
    Ipynb.ExecuteResult
      { executeCount,
        executeData = executeData,
        executeMetadata
      }
embedOutput _ (Nbparts.Err errName errValue errTraceback) = pure $ Ipynb.Err {errName, errValue, errTraceback}
