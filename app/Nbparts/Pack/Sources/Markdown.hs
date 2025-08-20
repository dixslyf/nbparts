{-# LANGUAGE OverloadedStrings #-}

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
import Data.Tuple qualified as Tuple
import Nbparts.Pack.Error qualified as Nbparts
import Nbparts.Pack.Mime qualified as Nbparts.Mime
import Nbparts.Types qualified as Nbparts
import Nbparts.Util.Markdown qualified
import Nbparts.Util.Text qualified
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

type Parser = Parsec Nbparts.ParseMarkdownSourcesError Text

markdownToSources :: String -> Text -> Either Nbparts.PackError [Nbparts.Source]
markdownToSources filename mdText = left Nbparts.PackParseMarkdownSourcesError $ P.runParser parseSources filename mdText

parseSources :: Parser [Nbparts.Source]
parseSources = P.many parseSource

parseSource :: Parser Nbparts.Source
parseSource = do
  (Nbparts.CellInfo cellId cellType maybeAttachmentUrls) <- parseCellInfo

  -- Try removing the newline added after the <!-- nbparts:cell ... --> comment.
  Monad.void $ P.optional P.newline

  srcText <- case cellType of
    Nbparts.Code -> parseCodeOrRawCell
    Nbparts.Raw -> parseCodeOrRawCell
    Nbparts.Markdown -> fixAttachments maybeAttachmentUrls <$> parseOtherCell
    _ -> parseOtherCell

  let src = Nbparts.Util.Text.splitKeepNewlines srcText

  let attachments = do
        (Nbparts.CellAttachmentUrls attachmentUrls) <- maybeAttachmentUrls
        -- Remove the "attachment:" prefix from attachment URLs to get the original attachment names.
        let attachmentNames = Map.map (\url -> Maybe.fromMaybe url $ Text.stripPrefix "attachment:" url) attachmentUrls
        -- We shouldn't have any duplicate keys since the mapping should be one-to-one.
        -- This is now a mapping from the original attachment names to their corresponding media file paths.
        let reversed = Map.fromList $ map Tuple.swap (Map.toList attachmentNames)
        pure $ Nbparts.UnembeddedMimeAttachments (Map.map Nbparts.Mime.mediaPathToMimeBundle reversed)

  pure $ Nbparts.Source cellType cellId src attachments

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
  body <- Text.pack <$> P.manyTill P.anySingle (P.lookAhead (Monad.void parseCellInfo <|> P.eof))
  -- Again, removing the trailing two newlines. Since parsing non-code blocks only stops
  -- when encountering the <!-- nbparts:cell ... --> comment, the parsed text contains the
  -- newlines, so we have to remove them using plain text functions instead of Megaparsec.
  pure $ Maybe.fromMaybe body $ Text.stripSuffix "\n\n" body

parseCellInfo :: Parser Nbparts.CellInfo
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

fixAttachments :: Maybe Nbparts.CellAttachmentUrls -> Text -> Text
fixAttachments maybeAttachmentUrls mdText = do
  case maybeAttachmentUrls of
    Just attachmentUrls ->
      let mdTree = CMarkGFM.commonmarkToNode [CMarkGFM.optSourcePos] [] mdText
          attachmentFixes = collectAttachmentFixes attachmentUrls mdTree
       in Nbparts.Util.Markdown.replaceSlices mdText attachmentFixes
    Nothing -> mdText

collectAttachmentFixes ::
  Nbparts.CellAttachmentUrls ->
  CMarkGFM.Node ->
  [(CMarkGFM.PosInfo, Text)]
collectAttachmentFixes
  (Nbparts.CellAttachmentUrls attachmentUrls)
  (CMarkGFM.Node maybePosInfo (CMarkGFM.IMAGE url title) children)
    | Maybe.isJust $ Map.lookup url attachmentUrls =
        let -- Safety: The guard guarantees that the url is in the map.
            originalUrl = Maybe.fromJust $ Map.lookup url attachmentUrls

            -- Safety: We should have parsed with the CMarkGFM.optSourcePos extension,
            -- so position information should be available.
            posInfo = Maybe.fromJust maybePosInfo

            fixedImageNode = CMarkGFM.Node Nothing (CMarkGFM.IMAGE originalUrl title) children
            fixedImageNodeText = Text.strip $ CMarkGFM.nodeToCommonmark [] Nothing fixedImageNode
         in [(posInfo, fixedImageNodeText)]
collectAttachmentFixes attachments (CMarkGFM.Node _ _ children) = foldMap (collectAttachmentFixes attachments) children

unescapeCellInfo :: Text -> Text
unescapeCellInfo = Text.replace "\\\\" "\\" . Text.replace "-\\->" "-->"
