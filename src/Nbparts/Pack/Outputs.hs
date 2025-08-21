module Nbparts.Pack.Outputs where

import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Nbparts.Pack.Mime qualified as Nbparts
import Nbparts.Types qualified as Nbparts

fillOutputs ::
  (MonadError Nbparts.PackError m, MonadIO m) =>
  FilePath ->
  Nbparts.UnembeddedNotebookOutputs ->
  Ipynb.Notebook a ->
  m (Ipynb.Notebook a)
fillOutputs prefixDir unembeddedOutputs (Ipynb.Notebook meta format cells) = do
  outputs <- liftIO $ (embedOutputs . adjustNotebookOutputPaths prefixDir) unembeddedOutputs
  filledCells <- liftEither $ traverse (fillCellOutputs outputs) cells
  return $ Ipynb.Notebook meta format filledCells

fillCellOutputs :: Nbparts.NotebookOutputs a -> Ipynb.Cell a -> Either Nbparts.PackError (Ipynb.Cell a)
fillCellOutputs (Nbparts.NotebookOutputs outputs) (Ipynb.Cell (Ipynb.Code codeExecutionCount _) maybeCellId source meta attachments) = do
  cellId <- maybe (Left Nbparts.PackMissingCellIdError) Right maybeCellId
  cellOutputs <- maybe (Left $ Nbparts.PackMissingCellOutputsError cellId) Right (Map.lookup cellId outputs)
  return $ Ipynb.Cell (Ipynb.Code codeExecutionCount cellOutputs) (Just cellId) source meta attachments
fillCellOutputs _ cell = pure cell

adjustNotebookOutputPaths :: FilePath -> Nbparts.UnembeddedNotebookOutputs -> Nbparts.UnembeddedNotebookOutputs
adjustNotebookOutputPaths prefixDir = coerce $ Map.map (map $ adjustCellOutputPaths prefixDir)

adjustCellOutputPaths :: FilePath -> Nbparts.UnembeddedCellOutput -> Nbparts.UnembeddedCellOutput
adjustCellOutputPaths prefixDir (Nbparts.DisplayData displayData metadata) =
  Nbparts.DisplayData (Nbparts.adjustMimeBundlePaths prefixDir displayData) metadata
adjustCellOutputPaths prefixDir (Nbparts.ExecuteResult count executeData metadata) =
  Nbparts.ExecuteResult count (Nbparts.adjustMimeBundlePaths prefixDir executeData) metadata
adjustCellOutputPaths _ output = output

embedOutputs :: Nbparts.UnembeddedNotebookOutputs -> IO (Nbparts.NotebookOutputs a)
embedOutputs = coerce $ fmap Nbparts.NotebookOutputs . traverse (mapM embedOutput)

embedOutput :: Nbparts.UnembeddedCellOutput -> IO (Ipynb.Output a)
embedOutput (Nbparts.Stream streamName streamText) = pure $ Ipynb.Stream {streamName, streamText = Ipynb.Source streamText}
embedOutput (Nbparts.DisplayData unembeddedDisplayData displayMetadata) = do
  displayData <- Nbparts.embedMimeBundle unembeddedDisplayData
  return
    Ipynb.DisplayData
      { displayData = displayData,
        displayMetadata
      }
embedOutput (Nbparts.ExecuteResult executeCount unembeddedExecuteData executeMetadata) = do
  executeData <- Nbparts.embedMimeBundle unembeddedExecuteData
  return
    Ipynb.ExecuteResult
      { executeCount,
        executeData = executeData,
        executeMetadata
      }
embedOutput (Nbparts.Err errName errValue errTraceback) = pure $ Ipynb.Err {errName, errValue, errTraceback}
