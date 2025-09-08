module Nbparts.Pack.Sources where

import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Nbparts.Pack.Mime (embedMimeAttachments)
import Nbparts.Types
  ( CellSource (CellSource),
    CellType (Code, Markdown, Raw),
  )

fillSources :: FilePath -> [CellSource] -> Ipynb.Notebook a -> IO (Ipynb.Notebook a)
fillSources readDir sources (Ipynb.Notebook meta format _) = do
  cells <- traverse (sourceToCell readDir) sources
  return $ Ipynb.Notebook meta format cells

sourceToCell :: FilePath -> CellSource -> IO (Ipynb.Cell a)
sourceToCell readDir (CellSource cellId cellType source attachments) = do
  let cellType' = case cellType of
        Markdown -> Ipynb.Markdown
        Raw -> Ipynb.Raw
        Code -> Ipynb.Code Nothing []
  attachments' <- traverse (embedMimeAttachments readDir) attachments
  return $ Ipynb.Cell cellType' (Just cellId) (Ipynb.Source source) (Ipynb.JSONMeta Map.empty) attachments'
