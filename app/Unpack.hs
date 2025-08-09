module Unpack where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, (-<.>), (<.>), (</>))
import Text.Pandoc (Block (Div), Inline (Image), Pandoc)
import Text.Pandoc.Class (PandocMonad, getMediaBag, runIO)
import Text.Pandoc.Class.IO (writeMedia)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.MediaBag (MediaBag, MediaItem (MediaItem), lookupMedia, mediaItems)
import Text.Pandoc.Options (WriterOptions (writerExtensions), def, pandocExtensions)
import Text.Pandoc.Readers.Ipynb
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Markdown (writeMarkdown)

unpack :: FilePath -> IO ()
unpack notebookPath = do
  notebookContents <- TIO.readFile notebookPath

  let outputDirectory = notebookPath <.> "nbparts"
  createDirectoryIfMissing True outputDirectory

  result <-
    runIO $
      do
        doc <- readIpynb def notebookContents
        mediaAdjustedDoc <- extractAuthoredMedia outputDirectory "media" doc
        let processedDoc = removeCellMetadata . removeCellOutputs $ mediaAdjustedDoc
        writeMarkdown def {writerExtensions = pandocExtensions} processedDoc
  markdown <- handleError result
  TIO.writeFile (outputDirectory </> (takeFileName notebookPath -<.> ".md")) markdown

removeCellMetadata :: Pandoc -> Pandoc
removeCellMetadata = walk filterCellMetadata
  where
    filterCellMetadata divBlock@(Div (identifier, classes, _attrs) blocks) | isCell divBlock = Div (identifier, classes, []) blocks
    filterCellMetadata block = block

isCell :: Block -> Bool
isCell (Div (_, classes, _) _) = T.pack "cell" `elem` classes
isCell _block = False

removeCellOutputs :: Pandoc -> Pandoc
removeCellOutputs = walk filterCellOutputs
  where
    filterCellOutputs (Div attr@(_, classes, _) blocks)
      | T.pack "cell" `elem` classes =
          Div attr filteredBlocks
      where
        filteredBlocks = filter (not . isOutputDiv) blocks
    filterCellOutputs block = block

isOutputDiv :: Block -> Bool
isOutputDiv (Div (_, classes, _) _) = T.pack "output" `elem` classes
isOutputDiv _block = False

-- Collects all output media sources in the document into a list.
listOutputMediaSrcs :: Pandoc -> [T.Text]
listOutputMediaSrcs = query divImageSrc
  where
    divImageSrc divBlock@(Div _ _) | isOutputDiv divBlock = query imageSrc divBlock
    divImageSrc _ = []
    imageSrc (Image _ _ (src, _)) = [src]
    imageSrc _ = []

-- Adapted from https://github.com/jgm/pandoc/blob/7639e800c5af85e5ded862a6e218d54489d17bfc/src/Text/Pandoc/Class/IO.hs#L236-L245
--
-- Pandoc's `extraMedia` unfortunately rewrites the source for images to always include the output directory as a prefix,
-- which is problematic since the rewritten paths will always be relative to the current directory (of the running nbparts)
-- instead of the output markdown file. The implementation below allows specifying a prefix; this prefix is prepended to the
-- output markdown file path, but not to the image sources.
extractAuthoredMedia :: (PandocMonad m, MonadIO m) => FilePath -> FilePath -> Pandoc -> m Pandoc
extractAuthoredMedia dirPrefix dir doc = do
  let outdir = dirPrefix </> dir
  media <- getMediaBag

  -- Filter out output media before writing.
  -- This runs in quadratic time, but shouldn't be too big of a deal
  -- unless there are a lot of media.
  let outputMediaSrcs = map T.unpack $ listOutputMediaSrcs doc
  let items = filter (\(src, _, _) -> src `notElem` outputMediaSrcs) (mediaItems media)

  if null items
    then return doc
    else do
      mapM_ (writeMedia outdir) items
      return $ walk (adjustImagePath dir media) doc

-- Adapted from https://github.com/jgm/pandoc/blob/7639e800c5af85e5ded862a6e218d54489d17bfc/src/Text/Pandoc/Class/IO.hs#L263-L273
adjustImagePath :: FilePath -> MediaBag -> Inline -> Inline
adjustImagePath dir mediabag image@(Image attr alt (src, title)) =
  case lookupMedia (T.unpack src) mediabag of
    Nothing -> image
    Just (MediaItem _ mediaPath _) ->
      let fullpath = dir </> mediaPath
       in Image attr alt (T.pack fullpath, title)
adjustImagePath _ _ inline = inline
