module Unpack where

import Data.Text.IO qualified as TIO
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Options (WriterOptions (writerExtensions), def, pandocExtensions)
import Text.Pandoc.Readers.Ipynb
import Text.Pandoc.Writers.Markdown (writeMarkdown)

unpack :: FilePath -> IO ()
unpack notebookPath = do
  notebookContents <- TIO.readFile notebookPath
  result <- runIO $ do
    doc <- readIpynb def notebookContents
    writeMarkdown def {writerExtensions = pandocExtensions} doc
  markdown <- handleError result
  TIO.writeFile (notebookPath <> ".md") markdown
