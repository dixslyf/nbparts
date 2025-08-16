module Nbparts.Unpack.Sources where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Ipynb qualified as Ipynb
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error qualified as Nbparts
import Nbparts.Unpack.Mime qualified as Nbparts

collectSources :: FilePath -> FilePath -> Ipynb.Notebook a -> IO (Either Nbparts.UnpackError [Nbparts.Source])
collectSources dirPrefix subdir (Ipynb.Notebook _ _ cells) = sequence <$> traverse (convertCell dirPrefix subdir) cells

convertCell :: FilePath -> FilePath -> Ipynb.Cell a -> IO (Either Nbparts.UnpackError Nbparts.Source)
convertCell dirPrefix subdir (Ipynb.Cell cellType maybeCellId (Ipynb.Source source) _ attachments) = runExceptT $ do
  unembeddedMimeAttachments <- liftIO $ traverse (Nbparts.unembedMimeAttachments dirPrefix subdir) attachments
  ExceptT $
    maybe
      (pure $ Left Nbparts.UnpackMissingCellIdError)
      (\cellId -> pure $ Right $ Nbparts.Source (convertCellType cellType) cellId source unembeddedMimeAttachments)
      maybeCellId

convertCellType :: Ipynb.CellType a -> Nbparts.CellType
convertCellType Ipynb.Markdown = Nbparts.Markdown
convertCellType (Ipynb.Heading headingLevel) = Nbparts.Heading headingLevel
convertCellType Ipynb.Raw = Nbparts.Raw
convertCellType (Ipynb.Code _ _) = Nbparts.Code
