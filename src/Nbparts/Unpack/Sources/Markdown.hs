module Nbparts.Unpack.Sources.Markdown where

import CMarkGFM qualified
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Nbparts.Types qualified as Nbparts
import Nbparts.Util.Map qualified
import Nbparts.Util.Markdown qualified

sourcesToMarkdown :: Text -> [Nbparts.CellSource] -> Either Nbparts.UnpackError Text
sourcesToMarkdown lang sources = do
  texts <- traverse (sourceToMarkdown lang) sources
  pure $ Text.concat (map (<> "\n\n") texts)

sourceToMarkdown :: Text -> Nbparts.CellSource -> Either Nbparts.UnpackError Text
sourceToMarkdown _ (Nbparts.CellSource cellId cellType@Nbparts.Markdown source maybeAttachments) = do
  -- TODO: Investigate options and extensions.
  let mdText = Text.concat source
  let mdTree = CMarkGFM.commonmarkToNode [CMarkGFM.optSourcePos] [] mdText
  (fixedMdText, maybeAttachmentNames) <- case maybeAttachments of
    Just attachments -> do
      (attachmentFixes, attachmentNames@(Nbparts.AttachmentNames innerAttachmentNames)) <- collectAttachmentFixes cellId attachments mdTree
      let fixedMdText = Nbparts.Util.Markdown.replaceSlices mdText attachmentFixes
      pure (fixedMdText, if Map.null innerAttachmentNames then Nothing else Just attachmentNames)
    Nothing -> pure (mdText, Nothing)
  pure $ cellStart (Nbparts.CellMarker cellId cellType maybeAttachmentNames) <> "\n" <> fixedMdText
sourceToMarkdown _ (Nbparts.CellSource cellId cellType@Nbparts.Raw source _) =
  pure $
    Text.intercalate
      "\n"
      [ cellStart (Nbparts.CellMarker cellId cellType Nothing),
        "```",
        Text.concat source,
        "```"
      ]
sourceToMarkdown lang (Nbparts.CellSource cellId cellType@Nbparts.Code source _) =
  pure $
    Text.intercalate
      "\n"
      [ cellStart (Nbparts.CellMarker cellId cellType Nothing),
        "```" <> lang,
        Text.concat source,
        "```"
      ]

cellStart :: Nbparts.CellMarker -> Text
cellStart cellInfo =
  Text.intercalate
    " "
    [ "<!--",
      "nbparts:cell",
      escapeCellInfo $ Text.decodeUtf8 $ LazyByteString.toStrict $ Aeson.encode cellInfo,
      "-->"
    ]

escapeCellInfo :: Text -> Text
escapeCellInfo = Text.replace "-->" "-\\->" . Text.replace "\\" "\\\\"

-- Because we want to maintain as much of the original formatting as possible,
-- instead of modifying the CMarkGFM AST and using its `nodeToCommonmark` function
-- (which would modify some of the formatting), we collect the positions of the image links
-- and what to replace them by before performing the replacement using plain text manipulation.
-- That way, we leave everything else untouched.
collectAttachmentFixes ::
  Text ->
  Nbparts.UnembeddedMimeAttachments ->
  CMarkGFM.Node ->
  Either Nbparts.UnpackError ([(CMarkGFM.PosInfo, Text)], Nbparts.AttachmentNames)
collectAttachmentFixes
  cellId
  (Nbparts.UnembeddedMimeAttachments attachments)
  (CMarkGFM.Node maybePosInfo (CMarkGFM.IMAGE url title) children)
    | Text.isPrefixOf "attachment:" url = do
        -- Safety: The guard already guarantees that "attachment:" is a prefix.
        let attachmentName = Maybe.fromJust $ Text.stripPrefix "attachment:" url

        let maybeAttachmentFp = do
              (Nbparts.UnembeddedMimeBundle mimeBundle) <- Map.lookup attachmentName attachments

              -- The mime bundle should only have 1 entry, but just in case it doesn't,
              -- we find the first entry whose mime type starts with "image".
              mimedata <- Nbparts.Util.Map.lookupByKeyPrefix "image" mimeBundle
              case mimedata of
                Nbparts.BinaryData fp -> pure fp
                _ -> Nothing

        attachmentFp <- case maybeAttachmentFp of
          Just fp -> pure $ Text.pack fp
          Nothing -> throwError $ Nbparts.UnpackMissingCellAttachmentError cellId attachmentName

        -- Safety: We should have parsed with the CMarkGFM.optSourcePos extension,
        -- so position information should be available.
        let posInfo = Maybe.fromJust maybePosInfo

        let fixedImageNode = CMarkGFM.Node Nothing (CMarkGFM.IMAGE attachmentFp title) children
        let fixedImageNodeText = Text.strip $ CMarkGFM.nodeToCommonmark [] Nothing fixedImageNode

        pure ([(posInfo, fixedImageNodeText)], Nbparts.AttachmentNames $ Map.singleton attachmentFp attachmentName)
collectAttachmentFixes cellId attachments (CMarkGFM.Node _ _ children) =
  foldl'
    (\(ps, Nbparts.AttachmentNames m) (accPs, Nbparts.AttachmentNames accM) -> (ps ++ accPs, Nbparts.AttachmentNames $ Map.union m accM))
    ([], Nbparts.AttachmentNames Map.empty)
    <$> unmergedFixes
  where
    unmergedFixes = traverse (collectAttachmentFixes cellId attachments) children
