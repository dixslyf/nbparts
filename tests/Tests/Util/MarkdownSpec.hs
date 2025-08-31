module Tests.Util.MarkdownSpec where

import Commonmark (SourceRange (SourceRange))
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (Gen, evalMaybe, forAll, success, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Util.Markdown (blockSourceRangeToIndices, sourceRangeToIndices)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Parsec (Column, Line, SourcePos)
import Text.Parsec.Pos (newPos)

mkSrcRange :: Line -> Column -> Line -> Column -> SourceRange
mkSrcRange startLine startCol endLine endCol = SourceRange [(mkPos startLine startCol, mkPos endLine endCol)]

mkPos :: Line -> Column -> SourcePos
mkPos = newPos ""

genTextLines :: Gen [Text]
genTextLines =
  Gen.list
    (Range.linear 1 5)
    (Gen.text (Range.linear 1 50) Gen.alphaNum) -- TODO: Figure out how to generate unicode text, but without newlines.

spec :: Spec
spec = do
  describe "sourceRangeToIndices" $ do
    context "when given an empty list" $ do
      it "returns Nothing" $ hedgehog $ do
        sLine <- forAll $ Gen.int (Range.linear minBound maxBound)
        sCol <- forAll $ Gen.int (Range.linear minBound maxBound)
        eLine <- forAll $ Gen.int (Range.linear minBound maxBound)
        eCol <- forAll $ Gen.int (Range.linear minBound maxBound)
        sourceRangeToIndices [] (mkSrcRange sLine sCol eLine eCol) === Nothing

    context "when given single-line text and a valid single-line span" $ do
      it "returns `startCol - 1` and `endCol - 1`" $ do
        textLine <- forAll $ Gen.text (Range.linear 1 50) Gen.unicode
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length textLine))
        eCol <- forAll $ Gen.int (Range.linear sCol (Text.length textLine + 1))
        sourceRangeToIndices [textLine] (mkSrcRange 1 sCol 1 eCol) === Just (sCol - 1, eCol - 1)

    context "when given multiple lines with valid line and col" $ do
      it "returns the correct indices for a simple case" $ do
        let mdLines = ["foo", "barbaz"]
        sourceRangeToIndices mdLines (mkSrcRange 1 2 2 6) `shouldBe` Just (1, 9)

      it "returns indices whose text span is equal to the one retrieved using the SourceRange" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear sLine (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear (if eLine == sLine then sCol else 1) (Text.length eTextLine + 1))

        let textSpan =
              if sLine == eLine
                then
                  sTextLine
                    & Text.drop (sCol - 1)
                    & Text.take (eCol - sCol)
                else
                  let spanStart = Text.drop (sCol - 1) sTextLine
                      spanMiddle = [txt | (line, txt) <- zip [1 ..] textLines, line > sLine, line < eLine]
                      spanEnd = Text.take (eCol - 1) eTextLine
                   in Text.intercalate "\n" ([spanStart] ++ spanMiddle ++ [spanEnd])

        let text = Text.intercalate "\n" textLines
        (sIdx, eIdx) <- evalMaybe $ sourceRangeToIndices textLines (mkSrcRange sLine sCol eLine eCol)
        let textSpan' =
              text
                & Text.drop sIdx
                & Text.take (eIdx - sIdx)

        textSpan === textSpan'

    context "when given non-positive indices" $ do
      it "returns Nothing for non-positive start indices" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear minBound 0)
        sCol <- forAll $ Gen.int (Range.linear minBound 0)

        eLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear 1 (Text.length eTextLine))

        sourceRangeToIndices textLines (mkSrcRange sLine sCol eLine eCol) === Nothing

      it "returns Nothing for non-positive end indices" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear minBound 0)
        eCol <- forAll $ Gen.int (Range.linear minBound 0)

        sourceRangeToIndices textLines (mkSrcRange sLine sCol eLine eCol) === Nothing

    context "when given out-of-bounds start indices" $ do
      it "returns Nothing" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear (length textLines + 1) maxBound)
        sCol <- forAll $ Gen.int (Range.linear 1 maxBound)

        eLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear 1 (Text.length eTextLine))

        sourceRangeToIndices textLines (mkSrcRange sLine sCol eLine eCol) === Nothing

    context "when given out-of-bounds end indices" $ do
      it "returns Nothing" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear (length textLines + 1) maxBound)
        eCol <- forAll $ Gen.int (Range.linear 1 maxBound)

        sourceRangeToIndices textLines (mkSrcRange sLine sCol eLine eCol) === Nothing

    context "when given start indices that are after end indices" $ do
      it "returns Nothing" $ hedgehog $ do
        textLines <- forAll genTextLines

        eLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear 1 (Text.length eTextLine))

        sLine <- forAll $ Gen.int (Range.linear eLine (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear (if sLine == eLine then eCol + 1 else 1) (Text.length sTextLine))

        if sLine == eLine && sCol == eCol
          then
            -- It's still possible to generate a valid set of start and end indices with the generators above,
            -- so we skip the check in that case.
            success
          else
            sourceRangeToIndices textLines (mkSrcRange sLine sCol eLine eCol) === Nothing

  describe "blockSourceRangeToIndices" $ do
    context "when given line and column valid for sourceRangeToIndices" $
      it "behaves like sourceRangeToIndices" $
        hedgehog $ do
          textLines <- forAll genTextLines

          sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
          let sTextLine = textLines !! (sLine - 1)
          sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

          eLine <- forAll $ Gen.int (Range.linear sLine (length textLines))
          let eTextLine = textLines !! (eLine - 1)
          eCol <- forAll $ Gen.int (Range.linear (if eLine == sLine then sCol else 1) (Text.length eTextLine + 1))

          blockSourceRangeToIndices textLines (mkSrcRange sLine sCol eLine eCol)
            === sourceRangeToIndices textLines (mkSrcRange sLine sCol eLine eCol)

    context "when given (lastLine + 1, 1) as the end index" $
      it "returns the length of the text as the end index" $
        hedgehog $ do
          textLines <- forAll genTextLines
          let text = Text.intercalate "\n" textLines

          sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
          let sTextLine = textLines !! (sLine - 1)
          sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

          (snd <$> blockSourceRangeToIndices textLines (mkSrcRange sLine sCol (length textLines + 1) 1))
            === Just (Text.length text)
