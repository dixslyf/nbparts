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
import Nbparts.Types
  ( CellMarker (CellMarker),
    CellSource (CellSource),
    CellType (Code, Markdown, Raw),
    UnembeddedMimeAttachments (UnembeddedMimeAttachments),
    UnembeddedMimeBundle (UnembeddedMimeBundle),
    UnembeddedMimeData (BinaryData),
    UnpackError (UnpackParseMarkdownError),
  )
import Nbparts.Util.Map qualified as MapUtil
import Nbparts.Util.Markdown qualified as MarkdownUtil
import Nbparts.Util.Text qualified as TextUtil

sourcesToMarkdown :: Text -> [CellSource] -> Either UnpackError Text
sourcesToMarkdown lang sources = do
  texts <- traverse (sourceToMarkdown lang) sources
  pure $ Text.concat (map (<> "\n\n") texts)

sourceToMarkdown :: Text -> CellSource -> Either UnpackError Text
sourceToMarkdown _ (CellSource cellId cellType@Markdown source maybeAttachments) = do
  -- NOTE: Remember that the elements in `source` have trailing newlines.
  let mdText = Text.concat source
  let mdLines = Text.lines mdText

  mdAst <- MarkdownUtil.parseMarkdown mdText & Arrow.left UnpackParseMarkdownError

  let escapesReplacements = MarkdownUtil.commentChangesWith escapeComments mdLines mdAst
  let attachmentReplacements = case maybeAttachments of
        Just attachments ->
          MarkdownUtil.attachmentChangesWith
            (fmap Text.pack . lookupAttachmentFilePath attachments)
            mdLines
            mdAst
        Nothing -> []
  let textReplacements = escapesReplacements <> attachmentReplacements

  -- Safety: The replacements do not overlap.
  let fixedMdText = Maybe.fromJust $ TextUtil.replaceSlices mdText textReplacements

  pure $ mkCellMarkerComment (CellMarker cellId cellType maybeAttachments) <> "\n" <> fixedMdText
sourceToMarkdown _ (CellSource cellId cellType@Raw source _) =
  pure $
    Text.intercalate
      "\n"
      [ mkCellMarkerComment (CellMarker cellId cellType Nothing),
        "```",
        Text.concat source,
        "```"
      ]
sourceToMarkdown lang (CellSource cellId cellType@Code source _) =
  pure $
    Text.intercalate
      "\n"
      [ mkCellMarkerComment (CellMarker cellId cellType Nothing),
        "```" <> lang,
        Text.concat source,
        "```"
      ]

mkCellMarkerComment :: CellMarker -> Text
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

lookupAttachmentFilePath :: UnembeddedMimeAttachments -> Text -> Maybe FilePath
lookupAttachmentFilePath (UnembeddedMimeAttachments attachments) target = do
  attachmentName <- Text.stripPrefix "attachment:" target

  -- TODO: Should warn if the attachment can't be found.
  (UnembeddedMimeBundle mimeBundle) <- Map.lookup attachmentName attachments

  -- The mime bundle should only have 1 entry, but just in case it doesn't,
  -- we find the first entry whose mime type starts with "image".
  mimedata <- MapUtil.lookupByKeyPrefix "image" mimeBundle
  case mimedata of
    BinaryData fp -> Just fp
    _ -> Nothing
