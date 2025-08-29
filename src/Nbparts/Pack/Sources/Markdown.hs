module Nbparts.Pack.Sources.Markdown where

import CMarkGFM qualified
import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (left)
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Nbparts.Types qualified as Nbparts
import Nbparts.Util.Markdown qualified
import Nbparts.Util.Text qualified
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

type Parser = Parsec Nbparts.ParseMarkdownSourcesError Text

markdownToSources :: String -> Text -> Either Nbparts.PackError [Nbparts.CellSource]
markdownToSources filename mdText = left Nbparts.PackParseMarkdownSourcesError $ P.runParser parseSources filename mdText

parseSources :: Parser [Nbparts.CellSource]
parseSources = P.many parseSource

parseSource :: Parser Nbparts.CellSource
parseSource = do
  (Nbparts.CellMarker cellId cellType maybeAttachments) <- parseCellInfo

  -- Try removing the newline added after the <!-- nbparts:cell ... --> comment.
  Monad.void $ P.optional P.newline

  srcText <- case cellType of
    Nbparts.Code -> parseCodeOrRawCell
    Nbparts.Raw -> parseCodeOrRawCell
    Nbparts.Markdown -> fixAttachments maybeAttachments <$> parseOtherCell

  let src = Nbparts.Util.Text.splitKeepNewlines srcText

  pure $ Nbparts.CellSource cellId cellType src maybeAttachments

parseCodeOrRawCell :: Parser Text
parseCodeOrRawCell = do
  code <- parseCodeBlock
  -- During unpacking, we appended two newlines to the end of the cell content for prettier output,
  -- so, now, we remove the newlines.
  Monad.void $ P.optional (P.newline >> P.newline)
  pure code

parseCodeBlock :: Parser Text
parseCodeBlock = do
  Monad.void $ P.string "```"
  Monad.void $ P.manyTill P.anySingle P.newline -- Ignore the language identifier.
  code <- Text.pack <$> P.manyTill P.anySingle (P.string "```")

  -- We appended a newline when unpacking, so remove it.
  let stripped = Maybe.fromMaybe code $ Text.stripSuffix "\n" code
  pure stripped

parseOtherCell :: Parser Text
parseOtherCell = do
  body <- Text.pack <$> P.manyTill P.anySingle (P.lookAhead (Monad.void (P.try parseCellInfo) <|> P.eof))
  -- Again, removing the trailing two newlines. Since parsing non-code blocks only stops
  -- when encountering the <!-- nbparts:cell ... --> comment, the parsed text contains the
  -- newlines, so we have to remove them using plain text functions instead of Megaparsec.
  pure $ Maybe.fromMaybe body $ Text.stripSuffix "\n\n" body

parseCellInfo :: Parser Nbparts.CellMarker
parseCellInfo = do
  Monad.void $ P.string "<!--"
  P.space
  Monad.void $ P.string "nbparts:cell"
  P.space1
  json <- Text.pack <$> P.manyTill P.anySingle (P.string "-->")
  let jsonUnescaped = unescapeCellInfo json
  case Aeson.eitherDecodeStrict (Text.encodeUtf8 jsonUnescaped) of
    Right cellInfo -> pure cellInfo
    Left err -> P.customFailure (Nbparts.ParseMarkdownSourcesJsonError $ Text.pack err)

fixAttachments :: Maybe Nbparts.UnembeddedMimeAttachments -> Text -> Text
fixAttachments maybeAttachments mdText = do
  case maybeAttachments of
    Just attachments ->
      let mdTree = CMarkGFM.commonmarkToNode [CMarkGFM.optSourcePos] [] mdText
          attachmentFixes = collectAttachmentFixes attachments mdTree
       in -- Safety: `collectAttachmentFixes` should always give valid replacement indices.
          Maybe.fromJust $ Nbparts.Util.Markdown.replaceSlices mdText attachmentFixes
    Nothing -> mdText

collectAttachmentFixes ::
  Nbparts.UnembeddedMimeAttachments ->
  CMarkGFM.Node ->
  [(CMarkGFM.PosInfo, Text)]
collectAttachmentFixes
  attachments
  (CMarkGFM.Node maybePosInfo (CMarkGFM.IMAGE url title) children)
    | Just attachmentName <- findAttachmentNameByFilePath (Text.unpack url) attachments =
        let -- Safety: We should have parsed with the CMarkGFM.optSourcePos extension,
            -- so position information should be available.
            posInfo = Maybe.fromJust maybePosInfo

            fixedImageNode = CMarkGFM.Node Nothing (CMarkGFM.IMAGE ("attachment:" <> attachmentName) title) children
            fixedImageNodeText = Text.strip $ CMarkGFM.nodeToCommonmark [] Nothing fixedImageNode
         in [(posInfo, fixedImageNodeText)]
collectAttachmentFixes attachments (CMarkGFM.Node _ _ children) = concatMap (collectAttachmentFixes attachments) children

findAttachmentNameByFilePath :: FilePath -> Nbparts.UnembeddedMimeAttachments -> Maybe Text
findAttachmentNameByFilePath targetFp (Nbparts.UnembeddedMimeAttachments attachments) =
  Map.foldrWithKey go Nothing attachments
  where
    go :: Text -> Nbparts.UnembeddedMimeBundle -> Maybe Text -> Maybe Text
    go attachmentName (Nbparts.UnembeddedMimeBundle bundle) acc =
      acc
        <|> if any (matches targetFp) (Map.elems bundle)
          then Just attachmentName
          else Nothing

    matches :: FilePath -> Nbparts.UnembeddedMimeData -> Bool
    matches targetFp' (Nbparts.BinaryData fp) = fp == targetFp'
    matches _ _ = False

unescapeCellInfo :: Text -> Text
unescapeCellInfo = Text.replace "\\\\" "\\" . Text.replace "-\\->" "-->"
