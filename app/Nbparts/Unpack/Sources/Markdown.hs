{-# LANGUAGE OverloadedStrings #-}

module Nbparts.Unpack.Sources.Markdown where

import CMarkGFM qualified
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error qualified as Nbparts

sourcesToMarkdown :: Text -> [Nbparts.Source] -> Either Nbparts.UnpackError Text
sourcesToMarkdown lang sources = do
  texts <- traverse (sourceToMarkdown lang) sources
  pure $ Text.concat (map (<> "\n\n") texts)

sourceToMarkdown :: Text -> Nbparts.Source -> Either Nbparts.UnpackError Text
sourceToMarkdown _ (Nbparts.Source cellType@Nbparts.Markdown cellId source maybeAttachments) = do
  -- TODO: Investigate options and extensions.
  let mdText = Text.concat source
  let mdTree = CMarkGFM.commonmarkToNode [CMarkGFM.optSourcePos] [] mdText
  attachmentFixes <- case maybeAttachments of
    Just attachments -> collectAttachmentFixes cellId attachments mdTree
    Nothing -> pure []
  let fixedMdText = applyAttachmentFixes attachmentFixes mdText
  pure $ cellStart cellId cellType <> "\n" <> fixedMdText
sourceToMarkdown _ (Nbparts.Source cellType@(Nbparts.Heading _) cellId source _) = pure $ cellStart cellId cellType <> "\n" <> Text.concat source
sourceToMarkdown _ (Nbparts.Source cellType@Nbparts.Raw cellId source _) = pure $ cellStart cellId cellType <> "\n" <> Text.concat source
sourceToMarkdown lang (Nbparts.Source cellType@Nbparts.Code cellId source _) =
  pure $
    cellStart cellId cellType
      <> "\n"
      <> "```"
      <> lang
      <> "\n"
      <> Text.concat source
      <> "\n```"

cellStart :: Text -> Nbparts.CellType -> Text
cellStart cellId cellType =
  Text.intercalate
    " "
    [ "<!--",
      "nbparts:begin-cell",
      "cell-id:" <> cellId,
      "cell-type:" <> case cellType of
        Nbparts.Markdown -> "markdown"
        Nbparts.Heading headingLevel -> "heading-" <> Text.show headingLevel
        Nbparts.Raw -> "raw"
        Nbparts.Code -> "code",
      "-->"
    ]

applyAttachmentFixes :: [(CMarkGFM.PosInfo, Text)] -> Text -> Text
applyAttachmentFixes attachmentFixes mdText =
  let mdLines = Text.lines mdText
      attachmentFixes' = map (Data.Bifunctor.first (posInfoToIndices mdLines)) attachmentFixes
   in replaceSlices mdText attachmentFixes'

-- Because we want to maintain as much of the original formatting as possible,
-- instead of modifying the CMarkGFM AST and using its `nodeToCommonmark` function
-- (which would modify some of the formatting), we collect the positions of the image links
-- and what to replace them by before performing the replacement using plain text manipulation.
-- That way, we leave everything else untouched.
collectAttachmentFixes :: Text -> Nbparts.UnembeddedMimeAttachments -> CMarkGFM.Node -> Either Nbparts.UnpackError [(CMarkGFM.PosInfo, Text)]
collectAttachmentFixes
  cellId
  (Nbparts.UnembeddedMimeAttachments attachments)
  (CMarkGFM.Node maybePosInfo (CMarkGFM.IMAGE url title) children)
    | Text.isPrefixOf "attachment:" url = do
        -- Safety: The guard already guarantees that "attachment:" is a prefix.
        let attachmentName = Maybe.fromJust $ Text.stripPrefix "attachment:" url

        let maybeAttachmentUrl = do
              mimeBundle <- Map.lookup attachmentName attachments

              -- The mime bundle should only have 1 entry, but just in case it doesn't,
              -- we find the first entry whose mime type starts with "image".
              mimedata <- lookupByKeyPrefix "image" mimeBundle
              case mimedata of
                Nbparts.BinaryData fp -> pure fp
                _ -> Nothing

        attachmentUrl <- case maybeAttachmentUrl of
          Just fp -> pure $ Text.pack fp
          Nothing -> throwError $ Nbparts.UnpackMissingCellAttachmentError cellId attachmentName

        -- Safety: We should have parsed with the CMarkGFM.optSourcePos extension,
        -- so position information should be available.
        let posInfo = Maybe.fromJust maybePosInfo

        let fixedImageNode = CMarkGFM.Node Nothing (CMarkGFM.IMAGE attachmentUrl title) children
        let fixedImageNodeText = Text.strip $ CMarkGFM.nodeToCommonmark [] Nothing fixedImageNode
        pure [(posInfo, fixedImageNodeText)]
collectAttachmentFixes cellId attachments (CMarkGFM.Node _ _ children) = join <$> traverse (collectAttachmentFixes cellId attachments) children

lookupByKeyPrefix :: Text -> Map Text v -> Maybe v
lookupByKeyPrefix prefix m =
  -- First entry after performing `dropWhile` is the first entry whose key starts with the prefix.
  case dropWhile (not . Text.isPrefixOf prefix . fst) entries of
    ((_key, value) : _) -> Just value
    [] -> Nothing
  where
    entries = Map.toList m

posInfoToIndices :: [Text] -> CMarkGFM.PosInfo -> (Int, Int)
posInfoToIndices mdLines (CMarkGFM.PosInfo startLine startColumn endLine endColumn) =
  ( lineColToIndex mdLines startLine startColumn,
    lineColToIndex mdLines endLine endColumn + 1 -- +1 because endColumn seems to be inclusive, but we want exclusive.
  )

lineColToIndex :: [Text] -> Int -> Int -> Int
lineColToIndex textLines line col =
  let linesBefore = take (line - 1) textLines
      -- +1 for each newline that `Text.lines` removed
      charsBefore = sum (map ((+ 1) . Text.length) linesBefore)
   in charsBefore + (col - 1)

replaceSlices :: Text -> [((Int, Int), Text)] -> Text
replaceSlices input = go 0
  where
    go :: Int -> [((Int, Int), Text)] -> Text
    go pos [] =
      Text.drop pos input
    go pos (((start, end), replacement) : rs) =
      let before = Text.take (start - pos) (Text.drop pos input)
          after = go end rs
       in before <> replacement <> after
