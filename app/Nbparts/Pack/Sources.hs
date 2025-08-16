module Nbparts.Pack.Sources where

import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Nbparts.Pack.Mime qualified as Nbparts
import Nbparts.Types qualified as Nbparts

fillSources :: FilePath -> Ipynb.Notebook a -> [Nbparts.Source] -> IO (Ipynb.Notebook a)
fillSources prefixDir (Ipynb.Notebook meta format _) sources = do
  cells <- traverse (sourceToCell prefixDir) sources
  return $ Ipynb.Notebook meta format cells

sourceToCell :: FilePath -> Nbparts.Source -> IO (Ipynb.Cell a)
sourceToCell prefixDir (Nbparts.Source cellType cellId source attachments) = do
  let cellType' = case cellType of
        Nbparts.Markdown -> Ipynb.Markdown
        (Nbparts.Heading headingLevel) -> Ipynb.Heading headingLevel
        Nbparts.Raw -> Ipynb.Raw
        Nbparts.Code -> Ipynb.Code Nothing []
  attachments' <- traverse (Nbparts.embedMimeAttachments . Nbparts.adjustMimeAttachmentsPaths prefixDir) attachments
  return $ Ipynb.Cell cellType' (Just cellId) (Ipynb.Source source) (Ipynb.JSONMeta Map.empty) attachments'
