module Nbparts.Pack.Sources where

import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Nbparts.Pack.Mime qualified as Nbparts
import Nbparts.Types qualified as Nbparts

fillSources :: FilePath -> [Nbparts.CellSource] -> Ipynb.Notebook a -> IO (Ipynb.Notebook a)
fillSources readDir sources (Ipynb.Notebook meta format _) = do
  cells <- traverse (sourceToCell readDir) sources
  return $ Ipynb.Notebook meta format cells

sourceToCell :: FilePath -> Nbparts.CellSource -> IO (Ipynb.Cell a)
sourceToCell readDir (Nbparts.CellSource cellId cellType source attachments) = do
  let cellType' = case cellType of
        Nbparts.Markdown -> Ipynb.Markdown
        Nbparts.Raw -> Ipynb.Raw
        Nbparts.Code -> Ipynb.Code Nothing []
  attachments' <- traverse (Nbparts.embedMimeAttachments readDir) attachments
  return $ Ipynb.Cell cellType' (Just cellId) (Ipynb.Source source) (Ipynb.JSONMeta Map.empty) attachments'
