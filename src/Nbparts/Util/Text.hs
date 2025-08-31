module Nbparts.Util.Text where

import Control.Monad qualified as Monad
import Data.Text (Text)
import Data.Text qualified as Text
import qualified Data.List as List
import qualified Data.Ord as Ord

lineColToIndex :: [Text] -> Int -> Int -> Maybe Int
lineColToIndex textLines line col
  | line <= 0 || col <= 0 = Nothing
  | otherwise = go 1 0 textLines
  where
    go :: Int -> Int -> [Text] -> Maybe Int
    go _ _ [] = Nothing -- Not enough lines.
    go currentLine charsBefore (l : ls)
      | currentLine == line =
          if col <= Text.length l + 1
            then Just $ charsBefore + col - 1
            else Nothing
      | otherwise =
          -- +1 to `Text.length l` for the newline `Text.lines` removed
          go (currentLine + 1) (charsBefore + Text.length l + 1) ls

-- NOTE: Returns Nothing when slices overlap.
replaceSlices :: Text -> [((Int, Int), Text)] -> Maybe Text
replaceSlices input replacements = go 0 sortedReplacements
  where
    sortedReplacements = List.sortBy (Ord.comparing fst) replacements

    go :: Int -> [((Int, Int), Text)] -> Maybe Text
    go pos [] = Just $ Text.drop pos input
    go pos (((start, end), replacement) : rs) = do
      -- Bounds checking.
      Monad.unless
        ( start >= 0
            && end >= 0
            && start <= end
            && start >= pos
        )
        Nothing

      let current = Text.drop pos input
          relStart = start - pos
          relEnd = end - pos

      let (before, remainder) = Text.splitAt relStart current

      -- If the length of the replaced text we got is less than
      -- the expected length of `relEnd - relStart`, that means
      -- the indices are out of range.
      let replaced = Text.take (relEnd - relStart) remainder
      Monad.when (Text.length replaced < relEnd - relStart) Nothing

      after <- go end rs
      Just $ before <> replacement <> after

splitKeepNewlines :: Text -> [Text]
splitKeepNewlines txt
  | Text.null txt = []
  | otherwise =
      let (before, rest) = Text.break (== '\n') txt
       in case Text.uncons rest of
            Just ('\n', rest') -> (before `Text.snoc` '\n') : splitKeepNewlines rest'
            _ -> [before]
