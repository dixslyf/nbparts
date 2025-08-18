{-# LANGUAGE OverloadedStrings #-}

module Nbparts.Unpack.Sources.Markdown where

import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Types qualified as Nbparts

sourcesToMarkdown :: Text -> [Nbparts.Source] -> Text
sourcesToMarkdown lang = Text.concat . map (\source -> sourceToMarkdown lang source <> "\n\n")

sourceToMarkdown :: Text -> Nbparts.Source -> Text
sourceToMarkdown _ (Nbparts.Source cellType@Nbparts.Markdown cellId source attachments) = cellStart cellId cellType <> "\n" <> Text.concat source
sourceToMarkdown _ (Nbparts.Source cellType@(Nbparts.Heading _) cellId source _) = cellStart cellId cellType <> "\n" <> Text.concat source
sourceToMarkdown _ (Nbparts.Source cellType@Nbparts.Raw cellId source _) = cellStart cellId cellType <> "\n" <> Text.concat source
sourceToMarkdown lang (Nbparts.Source cellType@Nbparts.Code cellId source _) =
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
