module Nbparts.Util.Markdown where

import Commonmark qualified
import Control.Monad.Identity (runIdentity)
import Data.Data (Data)
import Data.Data qualified as Data
import Data.Function ((&))
import Data.List.NonEmpty qualified as NonEmptyList
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
sourceRangeToIndices mdLines (Commonmark.SourceRange srcRange) = do
  let srcRange' = NonEmptyList.fromList srcRange
      (startPos, _) = NonEmptyList.head srcRange'
      (_, endPos) = NonEmptyList.last srcRange'
  startIdx <- sourcePosToIndices mdLines startPos
  endIdx <- sourcePosToIndices mdLines endPos
  if startIdx <= endIdx
    then
      Just (startIdx, endIdx)
    else
      Nothing

sourcePosToIndices :: [Text] -> Commonmark.SourcePos -> Maybe Int
sourcePosToIndices mdLines srcPos = do
  let line = Commonmark.sourceLine srcPos
      column = Commonmark.sourceColumn srcPos
  Util.Text.lineColToIndex mdLines line column

blockSourceRangeToIndices :: [Text] -> Commonmark.SourceRange -> Maybe (Int, Int)
blockSourceRangeToIndices mdLines (Commonmark.SourceRange srcRange) = do
  let srcRange' = NonEmptyList.fromList srcRange
      (startPos, _) = NonEmptyList.head srcRange'
      (_, endPos) = NonEmptyList.last srcRange'
  startIdx <- sourcePosToIndices mdLines startPos
  endIdx <- blockEndSourcePosToIndices mdLines endPos
  if startIdx <= endIdx
    then
      Just (startIdx, endIdx)
    else
      Nothing

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
    mdText = Text.intercalate "\n" mdLines

    go :: (Data b) => b -> Maybe [((Int, Int), Text)]
    go node
      | Just (srcRange, html) <- htmlSourceRangeAndText node,
        Text.isPrefixOf "<!--" html = do
          -- Inlines don't have the same quirk as blocks (the (number of lines + 1, 1) end index issue).
          -- Applying `blockSourceRangeToIndices` to the source range for inlines should exhibit the same
          -- behaviour as `sourceRangeToIndices`.
          indices@(startIdx, endIdx) <- blockSourceRangeToIndices mdLines srcRange
          -- Unfortunately, the `html` given by the Commonmark parser leaves out some whitespace for
          -- mutiline inline HTML, so we need to manually extract the full HTML from the original text.
          let fullHtml = mdText & Text.take endIdx & Text.drop startIdx
          pure [(indices, transformComment fullHtml)]
      | otherwise = concat <$> sequenceA (Data.gmapQ go node)

    htmlSourceRangeAndText :: (Data a) => a -> Maybe (Commonmark.SourceRange, Text)
    htmlSourceRangeAndText node
      | Just (NbpartsMd.Block (NbpartsMd.RawBlock (Commonmark.Format "html") html) srcRange _attrs) <- Data.cast node =
          Just (srcRange, html)
      | Just (NbpartsMd.Inline (NbpartsMd.RawInline (Commonmark.Format "html") html) srcRange _attrs) <- Data.cast node =
          Just (srcRange, html)
      | otherwise = Nothing

-- Because we want to maintain as much of the original formatting as possible,
-- instead of modifying the AST and turning it into text (which would modify some of the formatting
-- since the AST doesn't have enough information about the formatting), we collect the positions of
-- the image links and what to replace them with before performing the replacement using plain text
-- manipulation. That way, we leave everything else untouched.
attachmentChangesWith :: (Text -> Maybe Text) -> [Text] -> NbpartsMd.Blocks -> [((Int, Int), Text)]
attachmentChangesWith transformTarget mdLines = go
  where
    mdText = Text.intercalate "\n" mdLines
    go :: (Data b) => b -> [((Int, Int), Text)]
    go node
      | Just (NbpartsMd.Inline (NbpartsMd.Image target _title (NbpartsMd.Inlines ils)) srcRange _attrs) <- Data.cast node =
          let unSrcRange = NonEmptyList.fromList $ Commonmark.unSourceRange srcRange
              -- Find the end index of the image's inlines, or if there are no inlines,
              -- then just use the image's start index. This index is used as the starting
              -- point from which to search for the target text.
              -- Safety: This should never fail since the indices are valid.
              searchStart = Maybe.fromJust $ sourcePosToIndices mdLines $ case ils of
                _ :|> (NbpartsMd.Inline _ (Commonmark.SourceRange sr) _) -> snd $ last sr
                -- TODO: Add tests for this case (empty inlines).
                Sequence.Empty -> fst $ NonEmptyList.head unSrcRange
           in mkChange srcRange target searchStart
      | Just (NbpartsMd.Block (NbpartsMd.ReferenceLinkDefinition label (target, _title)) srcRange _attrs) <- Data.cast node =
          let unSrcRange = NonEmptyList.fromList $ Commonmark.unSourceRange srcRange
              -- Safety: This should never fail since the indices are valid.
              -- We're simply adding the length of the label to the start index of the reference link definition.
              -- The length of the label is added to "skip" the label.
              searchStart = Text.length label + Maybe.fromJust (sourcePosToIndices mdLines (fst $ NonEmptyList.head unSrcRange))
           in mkChange srcRange target searchStart
      | otherwise = concat (Data.gmapQ go node)

    -- Finds the start and end indices of an image target and constructs the replacement.
    --
    -- If `transformTarget` fails, then we know this is not an attachment target.
    -- `findSliceBetween` should never fail for a reference link definition. However,
    -- it can fail for an image, in which case the target must be defined in a reference link definition.
    -- In other words, there is no failure case for `mkChange`; hence we use `fromMaybe` to default to
    -- an empty list.
    mkChange :: Commonmark.SourceRange -> Text -> Int -> [((Int, Int), Text)]
    mkChange srcRange target searchStart = Maybe.fromMaybe [] $ do
      transformedTarget <- transformTarget target

      -- Now, we need to find the start and end indices of the image target.
      let unSrcRange = NonEmptyList.fromList $ Commonmark.unSourceRange srcRange
          -- Safety: This should never fail since the indices are valid.
          searchEnd = Maybe.fromJust . sourcePosToIndices mdLines . snd . NonEmptyList.last $ unSrcRange

      indices <- Util.Text.findSliceBetween searchStart searchEnd mdText target
      Just [(indices, transformedTarget)]
