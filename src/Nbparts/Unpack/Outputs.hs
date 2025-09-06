module Nbparts.Unpack.Outputs where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as State
import Data.ByteString (ByteString)
import Data.Ipynb qualified as Ipynb
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Mime qualified as Nbparts

collectOutputs ::
  FilePath ->
  Ipynb.Notebook a ->
  Either Nbparts.UnpackError (Nbparts.UnembeddedNotebookOutputs, [(FilePath, ByteString)])
collectOutputs subdir (Ipynb.Notebook _meta _format cells) = State.runStateT nbOutputsState []
  where
    nbOutputsState ::
      ( MonadState [(FilePath, ByteString)] m,
        MonadError Nbparts.UnpackError m
      ) =>
      m Nbparts.UnembeddedNotebookOutputs
    nbOutputsState = do
      let codeOutputs = Maybe.mapMaybe extractCodeOutputs cells
      entries <- traverse (uncurry (unembedCodeOutputs subdir)) codeOutputs
      pure (Nbparts.UnembeddedNotebookOutputs $ Map.fromList entries)

    extractCodeOutputs :: Ipynb.Cell a -> Maybe (Maybe Text, [Ipynb.Output a])
    extractCodeOutputs (Ipynb.Cell (Ipynb.Code _ outputs) maybeCellId _ _ _)
      | List.null outputs = Nothing -- Filter out cells with empty outputs to avoid serialising empty lists.
      | otherwise = Just (maybeCellId, outputs)
    extractCodeOutputs _ = Nothing

unembedCodeOutputs ::
  (MonadState [(FilePath, ByteString)] m, MonadError Nbparts.UnpackError m) =>
  FilePath ->
  Maybe Text ->
  [Ipynb.Output a] ->
  m (Text, [Nbparts.UnembeddedCellOutput])
unembedCodeOutputs subdir (Just cellId) outputs = (cellId,) <$> traverse (unembedOutput subdir) outputs
unembedCodeOutputs _ Nothing _ = throwError Nbparts.UnpackMissingCellIdError

unembedOutput :: (MonadState [(FilePath, ByteString)] m) => FilePath -> Ipynb.Output a -> m Nbparts.UnembeddedCellOutput
unembedOutput _ (Ipynb.Stream streamName (Ipynb.Source streamText)) = pure $ Nbparts.Stream streamName streamText
unembedOutput subdir (Ipynb.DisplayData displayData metadata) = do
  uDisplayData <- Nbparts.unembedMimeBundle subdir displayData
  pure $ Nbparts.DisplayData uDisplayData metadata
unembedOutput subdir (Ipynb.ExecuteResult executeCount executeData metadata) = do
  uExData <- Nbparts.unembedMimeBundle subdir executeData
  pure $ Nbparts.ExecuteResult executeCount uExData metadata
unembedOutput _ (Ipynb.Err errName errValue errTraceback) = pure $ Nbparts.Err errName errValue errTraceback
