module Nbparts.Unpack.Sources where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict qualified as State
import Data.ByteString (ByteString)
import Data.Ipynb qualified as Ipynb
import Nbparts.Types
  ( CellSource (CellSource),
    CellType (Code, Markdown, Raw),
    UnpackError (UnpackMissingCellIdError),
  )
import Nbparts.Unpack.Mime (unembedMimeAttachments)

collectSources :: FilePath -> Ipynb.Notebook a -> Either UnpackError ([CellSource], [(FilePath, ByteString)])
collectSources subdir (Ipynb.Notebook _ _ cells) = State.runStateT (traverse (convertCell subdir) cells) []

convertCell ::
  (MonadState [(FilePath, ByteString)] m, MonadError UnpackError m) =>
  FilePath ->
  Ipynb.Cell a ->
  m CellSource
convertCell subdir (Ipynb.Cell cellType maybeCellId (Ipynb.Source source) _ maybeAtts) = do
  cellId <- case maybeCellId of
    Just cId -> pure cId
    Nothing -> throwError UnpackMissingCellIdError
  maybeUAtts <- traverse (unembedMimeAttachments subdir) maybeAtts
  pure $ CellSource cellId (convertCellType cellType) source maybeUAtts

convertCellType :: Ipynb.CellType a -> CellType
convertCellType Ipynb.Markdown = Markdown
convertCellType Ipynb.Raw = Raw
convertCellType (Ipynb.Code _ _) = Code
convertCellType (Ipynb.Heading _) = error "Unexpected heading cell" -- Should not be possible since we only deal with V4 notebooks.
