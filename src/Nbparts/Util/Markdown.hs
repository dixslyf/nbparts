module Nbparts.Util.Markdown where

import Commonmark qualified
import Control.Monad.Identity (runIdentity)
import Data.Data (Data)
import Data.Data qualified as Data
import Data.Function ((&))
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Sequence
import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Types.Sources.Markdown qualified as NbpartsMd
import Nbparts.Util.Text qualified as Util.Text

-- TODO: Revisit syntax spec
parseMarkdown :: Text -> Either Commonmark.ParseError NbpartsMd.Blocks
parseMarkdown mdText =
  runIdentity $
    mdText
      & Commonmark.tokenize ""
      & Commonmark.parseCommonmarkWith Commonmark.defaultSyntaxSpec

sourceRangeToIndices :: [Text] -> Commonmark.SourceRange -> Maybe (Int, Int)
sourceRangeToIndices mdLines (Commonmark.SourceRange [(start, end)]) = do
  startIdx <- sourcePosToIndices mdLines start
  endIdx <- sourcePosToIndices mdLines end
  if startIdx <= endIdx
    then
      Just (startIdx, endIdx)
    else
      Nothing
sourceRangeToIndices _ (Commonmark.SourceRange _) = error "Unexpected multiple source ranges"

sourcePosToIndices :: [Text] -> Commonmark.SourcePos -> Maybe Int
sourcePosToIndices mdLines srcPos = do
  let line = Commonmark.sourceLine srcPos
      column = Commonmark.sourceColumn srcPos
  Util.Text.lineColToIndex mdLines line column

blockSourceRangeToIndices :: [Text] -> Commonmark.SourceRange -> Maybe (Int, Int)
blockSourceRangeToIndices mdLines (Commonmark.SourceRange [(start, end)]) = do
  startIdx <- sourcePosToIndices mdLines start
  endIdx <- blockEndSourcePosToIndices mdLines end
  if startIdx <= endIdx
    then
      Just (startIdx, endIdx)
    else
      Nothing
blockSourceRangeToIndices _ (Commonmark.SourceRange _) = error "Unexpected multiple source ranges"

blockEndSourcePosToIndices :: [Text] -> Commonmark.SourcePos -> Maybe Int
blockEndSourcePosToIndices mdLines srcPos = do
  let lineCount = length mdLines
  -- For the last block element, Commonmark seems to set the end index
  -- to (number of lines + 1, 1), which is a problematic as there is
  -- no `number of lines +1`th line.
  let rawLine = Commonmark.sourceLine srcPos
      rawColumn = Commonmark.sourceColumn srcPos
      (line, column) =
        if rawLine == lineCount + 1 && rawColumn == 1
          then
            (lineCount, Text.length (last mdLines) + 1)
          else
            (rawLine, rawColumn)
  Util.Text.lineColToIndex mdLines line column

commentChangesWith :: (Text -> Text) -> [Text] -> NbpartsMd.Blocks -> Maybe [((Int, Int), Text)]
commentChangesWith transformComment mdLines = go
  where
    go :: (Data b) => b -> Maybe [((Int, Int), Text)]
    go node
      | Just (maybeIndices, html) <- htmlSourceRangeAndText node,
        Text.isPrefixOf "<!--" html = do
          indices <- maybeIndices
          pure [(indices, transformComment html)]
      | otherwise = concat <$> sequenceA (Data.gmapQ go node)

    htmlSourceRangeAndText :: (Data a) => a -> Maybe (Maybe (Int, Int), Text)
    htmlSourceRangeAndText node
      | Just (NbpartsMd.Block (NbpartsMd.RawBlock (Commonmark.Format "html") html) srcRange _attrs) <- Data.cast node =
          Just (blockSourceRangeToIndices mdLines srcRange, html)
      | Just (NbpartsMd.Inline (NbpartsMd.RawInline (Commonmark.Format "html") html) srcRange _attrs) <- Data.cast node =
          Just (sourceRangeToIndices mdLines srcRange, html)
      | otherwise = Nothing

-- TODO: Check if this works for reference link images.
-- Because we want to maintain as much of the original formatting as possible,
-- instead of modifying the CMarkGFM AST and using its `nodeToCommonmark` function
-- (which would modify some of the formatting), we collect the positions of the image links
-- and what to replace them by before performing the replacement using plain text manipulation.
-- That way, we leave everything else untouched.
attachmentChangesWith :: (Text -> Maybe Text) -> [Text] -> NbpartsMd.Blocks -> Maybe [((Int, Int), Text)]
attachmentChangesWith transformTarget mdLines = go
  where
    mdText = Text.intercalate "\n" mdLines
    go :: (Data b) => b -> Maybe [((Int, Int), Text)]
    go node
      | Just (NbpartsMd.Inline (NbpartsMd.Image target _title (NbpartsMd.Inlines ils)) srcRange _attrs) <- Data.cast node =
          case transformTarget target of
            Just transformedTarget ->
              let -- Now, we need to find the start and end indices of the image target.

                  -- Find the end index of the image's inlines, or if there are no inlines,
                  -- then just use the image's start index. This index is used as the starting
                  -- point from which to search for the target text.
                  -- Safety: This should never fail since the indices are valid.
                  searchStart = Maybe.fromJust $ sourcePosToIndices mdLines $ case ils of
                    _ :|> (NbpartsMd.Inline _ (Commonmark.SourceRange sr) _) -> snd $ last sr
                    Sequence.Empty -> fst $ last (Commonmark.unSourceRange srcRange)

                  searchText = Text.drop searchStart mdText
                  (prefix, _haystack) = Text.breakOn target searchText

                  targetLength = Text.length target
                  startIdx = searchStart + Text.length prefix
                  endIdx = startIdx + targetLength
                  indices = (startIdx, endIdx)
               in Just [(indices, transformedTarget)]
            Nothing -> Just []
      | otherwise = concat <$> sequenceA (Data.gmapQ go node)
