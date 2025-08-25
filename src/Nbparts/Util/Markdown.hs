module Nbparts.Util.Markdown where

import CMarkGFM qualified
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Util.Text qualified

posInfoToIndices :: [Text] -> CMarkGFM.PosInfo -> Maybe (Int, Int)
posInfoToIndices mdLines (CMarkGFM.PosInfo startLine startColumn endLine endColumn) = do
  startIdx <- Nbparts.Util.Text.lineColToIndex mdLines startLine startColumn
  -- +1 because endColumn is inclusive, but we want exclusive.
  endIdx <- Nbparts.Util.Text.lineColToIndex mdLines endLine endColumn <&> (+ 1)

  if startIdx < endIdx
    then
      Just (startIdx, endIdx)
    else
      Nothing

replaceSlices :: Text -> [(CMarkGFM.PosInfo, Text)] -> Maybe Text
replaceSlices mdText replacements =
  let mdLines = Text.lines mdText
      replacements' = traverse (\(r, txt) -> (,txt) <$> posInfoToIndices mdLines r) replacements
   in replacements' >>= Nbparts.Util.Text.replaceSlices mdText
