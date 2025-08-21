module Nbparts.Util.Markdown where

import CMarkGFM qualified
import Data.Bifunctor qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Util.Text qualified

posInfoToIndices :: [Text] -> CMarkGFM.PosInfo -> (Int, Int)
posInfoToIndices mdLines (CMarkGFM.PosInfo startLine startColumn endLine endColumn) =
  ( Nbparts.Util.Text.lineColToIndex mdLines startLine startColumn,
    Nbparts.Util.Text.lineColToIndex mdLines endLine endColumn + 1 -- +1 because endColumn seems to be inclusive, but we want exclusive.
  )

replaceSlices :: Text -> [(CMarkGFM.PosInfo, Text)] -> Text
replaceSlices mdText replacements =
  let mdLines = Text.lines mdText
      replacements' = map (Data.Bifunctor.first (posInfoToIndices mdLines)) replacements
   in Nbparts.Util.Text.replaceSlices mdText replacements'
