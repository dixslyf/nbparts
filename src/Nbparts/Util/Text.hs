module Nbparts.Util.Text where

import Data.Text (Text)
import Data.Text qualified as Text

lineColToIndex :: [Text] -> Int -> Int -> Int
lineColToIndex textLines line col =
  let linesBefore = take (line - 1) textLines
      -- +1 for each newline that `Text.lines` removed
      charsBefore = sum (map ((+ 1) . Text.length) linesBefore)
   in charsBefore + (col - 1)

-- NOTE: Assumes indices do not overlap.
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

splitKeepNewlines :: Text -> [Text]
splitKeepNewlines txt
  | Text.null txt = []
  | otherwise =
      let (before, rest) = Text.break (== '\n') txt
       in case Text.uncons rest of
            Just ('\n', rest') -> (before `Text.snoc` '\n') : splitKeepNewlines rest'
            _ -> [before]
