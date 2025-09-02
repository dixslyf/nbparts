module Nbparts.Unpack.Sources.Markdown where

import Control.Arrow qualified as Arrow
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Nbparts.Types qualified as Nbparts
import Nbparts.Util.Map qualified
import Nbparts.Util.Markdown qualified as Util.Markdown
import Nbparts.Util.Text qualified

sourcesToMarkdown :: Text -> [Nbparts.CellSource] -> Either Nbparts.UnpackError Text
sourcesToMarkdown lang sources = do
  texts <- traverse (sourceToMarkdown lang) sources
  pure $ Text.concat (map (<> "\n\n") texts)

sourceToMarkdown :: Text -> Nbparts.CellSource -> Either Nbparts.UnpackError Text
sourceToMarkdown _ (Nbparts.CellSource cellId cellType@Nbparts.Markdown source maybeAttachments) = do
  -- NOTE: Remember that the elements in `source` have trailing newlines.
  let mdText = Text.concat source
  let mdLines = Text.lines mdText

  mdAst <- Util.Markdown.parseMarkdown mdText & Arrow.left Nbparts.UnpackParseMarkdownError

  let escapesReplacements = Util.Markdown.commentChangesWith escapeComments mdLines mdAst
  let attachmentReplacements = case maybeAttachments of
        Just attachments ->
          Util.Markdown.attachmentChangesWith
            (fmap Text.pack . lookupAttachmentFilePath attachments)
            mdLines
            mdAst
        Nothing -> []
  let textReplacements = escapesReplacements <> attachmentReplacements

  -- Safety: The replacements do not overlap.
  let fixedMdText = Maybe.fromJust $ Nbparts.Util.Text.replaceSlices mdText textReplacements

  pure $ mkCellMarkerComment (Nbparts.CellMarker cellId cellType maybeAttachments) <> "\n" <> fixedMdText
sourceToMarkdown _ (Nbparts.CellSource cellId cellType@Nbparts.Raw source _) =
  pure $
    Text.intercalate
      "\n"
      [ mkCellMarkerComment (Nbparts.CellMarker cellId cellType Nothing),
        "```",
        Text.concat source,
        "```"
      ]
sourceToMarkdown lang (Nbparts.CellSource cellId cellType@Nbparts.Code source _) =
  pure $
    Text.intercalate
      "\n"
      [ mkCellMarkerComment (Nbparts.CellMarker cellId cellType Nothing),
        "```" <> lang,
        Text.concat source,
        "```"
      ]

mkCellMarkerComment :: Nbparts.CellMarker -> Text
mkCellMarkerComment cm =
  Text.intercalate
    " "
    [ "<!--",
      "nbparts:cell",
      escapeCellMarkerContent $ Text.decodeUtf8 $ LazyByteString.toStrict $ Aeson.encode cm,
      "-->"
    ]

escapeCellMarkerContent :: Text -> Text
escapeCellMarkerContent = Text.replace "-->" "-\\->" . Text.replace "\\" "\\\\"

escapeComments :: Text -> Text
escapeComments = Text.replace "nbparts:cell" "\\nbparts:cell" . Text.replace "\\" "\\\\"

lookupAttachmentFilePath :: Nbparts.UnembeddedMimeAttachments -> Text -> Maybe FilePath
lookupAttachmentFilePath (Nbparts.UnembeddedMimeAttachments attachments) target = do
  attachmentName <- Text.stripPrefix "attachment:" target

  -- TODO: Should warn if the attachment can't be found.
  (Nbparts.UnembeddedMimeBundle mimeBundle) <- Map.lookup attachmentName attachments

  -- The mime bundle should only have 1 entry, but just in case it doesn't,
  -- we find the first entry whose mime type starts with "image".
  mimedata <- Nbparts.Util.Map.lookupByKeyPrefix "image" mimeBundle
  case mimedata of
    Nbparts.BinaryData fp -> Just fp
    _ -> Nothing
