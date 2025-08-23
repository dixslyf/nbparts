module Nbparts.Util.Markdown where

import CMarkGFM qualified
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Nbparts.Util.Text qualified

posInfoToIndices :: [Text] -> CMarkGFM.PosInfo -> Maybe (Int, Int)
posInfoToIndices mdLines (CMarkGFM.PosInfo startLine startColumn endLine endColumn) =
  (,)
    <$> Nbparts.Util.Text.lineColToIndex mdLines startLine startColumn
    <*> (Nbparts.Util.Text.lineColToIndex mdLines endLine endColumn <&> (+ 1)) -- +1 because endColumn is inclusive, but we want exclusive.

replaceSlices :: Text -> [(CMarkGFM.PosInfo, Text)] -> Maybe Text
replaceSlices mdText replacements =
  let mdLines = Text.lines mdText
      replacements' = traverse (\(r, txt) -> (,txt) <$> posInfoToIndices mdLines r) replacements
   in replacements' >>= Nbparts.Util.Text.replaceSlices mdText
