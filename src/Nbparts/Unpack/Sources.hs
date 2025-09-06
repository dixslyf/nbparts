module Nbparts.Unpack.Sources where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict qualified as State
import Data.ByteString (ByteString)
import Data.Ipynb qualified as Ipynb
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Mime qualified as Nbparts

collectSources :: FilePath -> Ipynb.Notebook a -> Either Nbparts.UnpackError ([Nbparts.CellSource], [(FilePath, ByteString)])
collectSources subdir (Ipynb.Notebook _ _ cells) = State.runStateT (traverse (convertCell subdir) cells) []

convertCell ::
  (MonadState [(FilePath, ByteString)] m, MonadError Nbparts.UnpackError m) =>
  FilePath ->
  Ipynb.Cell a ->
  m Nbparts.CellSource
convertCell subdir (Ipynb.Cell cellType maybeCellId (Ipynb.Source source) _ maybeAtts) = do
  cellId <- case maybeCellId of
    Just cId -> pure cId
    Nothing -> throwError Nbparts.UnpackMissingCellIdError
  maybeUAtts <- traverse (Nbparts.unembedMimeAttachments subdir) maybeAtts
  pure $ Nbparts.CellSource cellId (convertCellType cellType) source maybeUAtts

convertCellType :: Ipynb.CellType a -> Nbparts.CellType
convertCellType Ipynb.Markdown = Nbparts.Markdown
convertCellType Ipynb.Raw = Nbparts.Raw
convertCellType (Ipynb.Code _ _) = Nbparts.Code
convertCellType (Ipynb.Heading _) = error "Unexpected heading cell" -- Should not be possible since we only deal with V4 notebooks.
