module Nbparts.Unpack.Outputs where

import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Nbparts.Types (UnembeddedOutput (..))
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error (UnpackError)
import Nbparts.Unpack.Error qualified as Nbparts
import Nbparts.Unpack.Mime qualified as Nbparts

collectOutputs :: Ipynb.Notebook a -> Either UnpackError (Nbparts.Outputs a)
collectOutputs (Ipynb.Notebook _meta _format cells) = Map.fromList <$> sequence (Maybe.mapMaybe toEntry cells)
  where
    toEntry :: Ipynb.Cell a -> Maybe (Either UnpackError (Text, [Ipynb.Output a]))
    toEntry (Ipynb.Cell (Ipynb.Code _exeCount outputs) maybeCellId _ _ _) = Just $ case maybeCellId of
      Just cellId -> Right (cellId, outputs)
      Nothing -> Left Nbparts.UnpackMissingCellIdError
    toEntry _ = Nothing

unembedOutputs :: FilePath -> FilePath -> Nbparts.Outputs a -> IO Nbparts.UnembeddedOutputs
unembedOutputs dirPrefix subdir = traverse $ mapM (unembedOutput dirPrefix subdir)

unembedOutput :: FilePath -> FilePath -> Ipynb.Output a -> IO Nbparts.UnembeddedOutput
unembedOutput _ _ (Ipynb.Stream streamName (Ipynb.Source streamText)) = pure $ Nbparts.Stream {streamName, streamText}
unembedOutput dirPrefix subdir (Ipynb.DisplayData displayData displayMetadata) = do
  unembeddedDisplayData <- Nbparts.unembedMimeBundle dirPrefix subdir displayData
  return
    DisplayData
      { displayData = unembeddedDisplayData,
        displayMetadata
      }
unembedOutput dirPrefix subdir (Ipynb.ExecuteResult executeCount executeData executeMetadata) = do
  unembeddedExecuteData <- Nbparts.unembedMimeBundle dirPrefix subdir executeData
  return
    ExecuteResult
      { executeCount,
        executeData = unembeddedExecuteData,
        executeMetadata
      }
unembedOutput _ _ (Ipynb.Err errName errValue errTraceback) = pure $ Err {errName, errValue, errTraceback}
