module Nbparts.Unpack.Sources where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ipynb qualified as Ipynb
import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error qualified as Nbparts
import Nbparts.Unpack.Mime qualified as Nbparts
import System.FilePath
import Text.Pandoc (Pandoc)
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Class.IO qualified as PandocIO
import Text.Pandoc.MediaBag qualified as Pandoc
import Text.Pandoc.Walk qualified as Pandoc

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

removeCellMetadata :: Pandoc -> Pandoc
removeCellMetadata = Pandoc.walk filterCellMetadata
  where
    filterCellMetadata divBlock@(Pandoc.Div (identifier, classes, _attrs) blocks) | isCell divBlock = Pandoc.Div (identifier, classes, []) blocks
    filterCellMetadata block = block

isCell :: Pandoc.Block -> Bool
isCell (Pandoc.Div (_, classes, _) _) = Text.pack "cell" `elem` classes
isCell _block = False

removeCellOutputs :: Pandoc -> Pandoc
removeCellOutputs = Pandoc.walk filterCellOutputs
  where
    filterCellOutputs (Pandoc.Div attr@(_, classes, _) blocks)
      | Text.pack "cell" `elem` classes =
          Pandoc.Div attr filteredBlocks
      where
        filteredBlocks = filter (not . isOutputDiv) blocks
    filterCellOutputs block = block

isOutputDiv :: Pandoc.Block -> Bool
isOutputDiv (Pandoc.Div (_, classes, _) _) = Text.pack "output" `elem` classes
isOutputDiv _block = False

-- Collects all output media sources in the document into a list.
listOutputMediaSrcs :: Pandoc -> [Text]
listOutputMediaSrcs = Pandoc.query divImageSrc
  where
    divImageSrc divBlock@(Pandoc.Div _ _) | isOutputDiv divBlock = Pandoc.query imageSrc divBlock
    divImageSrc _ = []
    imageSrc (Pandoc.Image _ _ (src, _)) = [src]
    imageSrc _ = []

-- Adapted from https://github.com/jgm/pandoc/blob/7639e800c5af85e5ded862a6e218d54489d17bfc/src/Text/Pandoc/Class/IO.hs#L236-L245
--
-- Pandoc's `extractMedia` unfortunately rewrites the source for images to always include the output directory as a prefix,
-- which is problematic since the rewritten paths will always be relative to the current directory (of the running nbparts)
-- instead of the output markdown file. The implementation below allows specifying a prefix; this prefix is prepended to the
-- output markdown file path, but not to the image sources.
extractSourceMedia :: (Pandoc.PandocMonad m, MonadIO m) => FilePath -> FilePath -> Pandoc -> m Pandoc
extractSourceMedia dirPrefix dir doc = do
  let outdir = dirPrefix </> dir
  media <- Pandoc.getMediaBag

  -- Filter out output media before writing.
  -- This runs in quadratic time, but shouldn't be too big of a deal
  -- unless there are a lot of media.
  let outputMediaSrcs = map Text.unpack $ listOutputMediaSrcs doc
  let items = filter (\(src, _, _) -> src `notElem` outputMediaSrcs) (Pandoc.mediaItems media)

  if null items
    then return doc
    else do
      mapM_ (PandocIO.writeMedia outdir) items
      return $ Pandoc.walk (adjustImagePath dir media) doc

-- Adapted from https://github.com/jgm/pandoc/blob/7639e800c5af85e5ded862a6e218d54489d17bfc/src/Text/Pandoc/Class/IO.hs#L263-L273
adjustImagePath :: FilePath -> Pandoc.MediaBag -> Pandoc.Inline -> Pandoc.Inline
adjustImagePath dir mediabag image@(Pandoc.Image attr alt (src, title)) =
  case Pandoc.lookupMedia (Text.unpack src) mediabag of
    Nothing -> image
    Just (Pandoc.MediaItem _ mediaPath _) ->
      let fullpath = dir </> mediaPath
       in Pandoc.Image attr alt (Text.pack fullpath, title)
adjustImagePath _ _ inline = inline
