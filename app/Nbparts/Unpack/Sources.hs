module Nbparts.Unpack.Sources where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ipynb qualified as Ipynb
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error qualified as Nbparts
import Nbparts.Unpack.Mime qualified as Nbparts

collectSources :: (MonadError Nbparts.UnpackError m, MonadIO m) => FilePath -> FilePath -> Ipynb.Notebook a -> m [Nbparts.Source]
collectSources dirPrefix subdir (Ipynb.Notebook _ _ cells) = traverse (convertCell dirPrefix subdir) cells

convertCell :: (MonadError Nbparts.UnpackError m, MonadIO m) => FilePath -> FilePath -> Ipynb.Cell a -> m Nbparts.Source
convertCell dirPrefix subdir (Ipynb.Cell cellType maybeCellId (Ipynb.Source source) _ attachments) = do
  unembeddedMimeAttachments <- liftIO $ traverse (Nbparts.unembedMimeAttachments dirPrefix subdir) attachments
  case maybeCellId of
    Just cellId -> pure $ Nbparts.Source (convertCellType cellType) cellId source unembeddedMimeAttachments
    Nothing -> throwError Nbparts.UnpackMissingCellIdError

convertCellType :: Ipynb.CellType a -> Nbparts.CellType
convertCellType Ipynb.Markdown = Nbparts.Markdown
convertCellType (Ipynb.Heading headingLevel) = Nbparts.Heading headingLevel
convertCellType Ipynb.Raw = Nbparts.Raw
convertCellType (Ipynb.Code _ _) = Nbparts.Code
