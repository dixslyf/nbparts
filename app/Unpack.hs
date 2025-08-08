module Unpack where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Pandoc (Block (Div))
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Options (WriterOptions (writerExtensions), def, pandocExtensions)
import Text.Pandoc.Readers.Ipynb
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Markdown (writeMarkdown)

unpack :: FilePath -> IO ()
unpack notebookPath = do
  notebookContents <- TIO.readFile notebookPath
  result <- runIO $ do
    doc <- readIpynb def notebookContents
    let processedDoc = walk filterCellOutputs doc
    writeMarkdown def {writerExtensions = pandocExtensions} processedDoc
  markdown <- handleError result
  TIO.writeFile (notebookPath <> ".md") markdown

filterCellOutputs :: Block -> Block
filterCellOutputs (Div attr@(_, classes, _) blocks)
  | T.pack "cell" `elem` classes =
      Div attr filteredBlocks
  where
    filteredBlocks = filter (not . isOutputDiv) blocks
filterCellOutputs block = block

isOutputDiv :: Block -> Bool
isOutputDiv (Div (_, classes, _) _) = T.pack "output" `elem` classes
isOutputDiv _block = False
