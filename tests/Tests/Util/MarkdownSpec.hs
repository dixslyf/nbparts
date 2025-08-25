module Tests.Util.MarkdownSpec where

import CMarkGFM qualified
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (Gen, evalMaybe, forAll, success, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Util.Markdown (posInfoToIndices, replaceSlices)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

genTextLines :: Gen [Text]
genTextLines =
  Gen.list
    (Range.linear 1 5)
    (Gen.text (Range.linear 1 50) Gen.unicode)

spec :: Spec
spec = do
  describe "posInfoToIndices" $ do
    context "when given an empty list" $ do
      it "returns Nothing" $ hedgehog $ do
        sLine <- forAll $ Gen.int (Range.linear minBound maxBound)
        sCol <- forAll $ Gen.int (Range.linear minBound maxBound)
        eLine <- forAll $ Gen.int (Range.linear minBound maxBound)
        eCol <- forAll $ Gen.int (Range.linear minBound maxBound)
        posInfoToIndices [] (CMarkGFM.PosInfo sLine sCol eLine eCol) === Nothing

    context "when given single-line text and a valid single-line span" $ do
      it "returns `startCol - 1` and `endCol`" $ do
        textLine <- forAll $ Gen.text (Range.linear 1 50) Gen.unicode
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length textLine))
        eCol <- forAll $ Gen.int (Range.linear sCol (Text.length textLine))
        posInfoToIndices [textLine] (CMarkGFM.PosInfo 1 sCol 1 eCol) === Just (sCol - 1, eCol)

    context "when given multiple lines with valid line and col" $ do
      it "returns the correct indices for a simple case" $ do
        let mdLines = ["foo", "barbaz"]
        posInfoToIndices mdLines (CMarkGFM.PosInfo 1 2 2 5) `shouldBe` Just (1, 9)

      it "returns indices whose text span is equal to the one retrieved using the PosInfo" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear sLine (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear (if eLine == sLine then sCol else 1) (Text.length eTextLine))

        let textSpan =
              if sLine == eLine
                then
                  sTextLine
                    & Text.drop (sCol - 1)
                    & Text.take (eCol - sCol + 1)
                else
                  let spanStart = Text.drop (sCol - 1) sTextLine
                      spanMiddle = [txt | (line, txt) <- zip [1 ..] textLines, line > sLine, line < eLine]
                      spanEnd = Text.take eCol eTextLine
                   in Text.intercalate "\n" ([spanStart] ++ spanMiddle ++ [spanEnd])

        let text = Text.intercalate "\n" textLines
        (sIdx, eIdx) <- evalMaybe $ posInfoToIndices textLines (CMarkGFM.PosInfo sLine sCol eLine eCol)
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

        posInfoToIndices textLines (CMarkGFM.PosInfo sLine sCol eLine eCol) === Nothing

      it "returns Nothing for non-positive end indices" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear minBound 0)
        eCol <- forAll $ Gen.int (Range.linear minBound 0)

        posInfoToIndices textLines (CMarkGFM.PosInfo sLine sCol eLine eCol) === Nothing

    context "when given out-of-bounds start indices" $ do
      it "returns Nothing" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear (length textLines + 1) maxBound)
        sCol <- forAll $ Gen.int (Range.linear 1 maxBound)

        eLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear 1 (Text.length eTextLine))

        posInfoToIndices textLines (CMarkGFM.PosInfo sLine sCol eLine eCol) === Nothing

    context "when given out-of-bounds end indices" $ do
      it "returns Nothing" $ hedgehog $ do
        textLines <- forAll genTextLines

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear (length textLines + 1) maxBound)
        eCol <- forAll $ Gen.int (Range.linear 1 maxBound)

        posInfoToIndices textLines (CMarkGFM.PosInfo sLine sCol eLine eCol) === Nothing

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
            posInfoToIndices textLines (CMarkGFM.PosInfo sLine sCol eLine eCol) === Nothing

  describe "replaceSlices" $ do
    context "when given no replacements" $ do
      it "is identity" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 0 250) Gen.unicode
        replaceSlices t [] === Just t

    context "when given only one single-line replacement" $ do
      it "behaves like manual splice" $ hedgehog $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        line <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let textLine = textLines !! (line - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length textLine))
        eCol <- forAll $ Gen.int (Range.linear sCol (Text.length textLine))

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        let posInfo = CMarkGFM.PosInfo line sCol line eCol
        let manual =
              Text.intercalate "\n" $
                take (line - 1) textLines
                  ++ [Text.take (sCol - 1) textLine <> repl <> Text.drop eCol textLine]
                  ++ drop line textLines
        let replaced = replaceSlices text [(posInfo, repl)]
        replaced === Just manual

    context "when given a single replacement that covers the whole text" $ do
      it "returns the replacement text" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        let eLine = length textLines
        let eCol = Text.length $ textLines !! (eLine - 1)

        repl <- forAll $ Gen.text (Range.linear 0 250) Gen.unicode

        replaceSlices text [(CMarkGFM.PosInfo 1 1 eLine eCol, repl)] === Just repl

    context "when given replacements across lines" $ do
      it "correctly replaces" $ do
        replaceSlices
          "abcd\nefghji"
          [(CMarkGFM.PosInfo 1 3 2 2, "FOO")]
          `shouldBe` Just "abFOOghji"

        replaceSlices
          "abcd\nefghji\nklmnop"
          [(CMarkGFM.PosInfo 1 3 2 2, "FOO"),
           (CMarkGFM.PosInfo 2 4 3 3, "BAR")
          ]
          `shouldBe` Just "abFOOgBARnop"

    context "when given non-overlapping slices" $ do
      it "correctly replaces" $ do
        replaceSlices
          "abcd\nefghji"
          [ (CMarkGFM.PosInfo 1 2 1 3, "FOO"),
            (CMarkGFM.PosInfo 1 4 1 4, "BAR"),
            (CMarkGFM.PosInfo 2 2 2 4, "BAZ")
          ]
          `shouldBe` Just "aFOOBAR\neBAZji"

    context "when given overlapping slices" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        sLine1 <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine1 = textLines !! (sLine1 - 1)
        sCol1 <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine1))

        eLine1 <- forAll $ Gen.int (Range.linear sLine1 (length textLines))
        let eTextLine1 = textLines !! (eLine1 - 1)
        eCol1 <- forAll $ Gen.int (Range.linear (if eLine1 == sLine1 then sCol1 else 1) (Text.length eTextLine1))

        sLine2 <- forAll $ Gen.int (Range.linear sLine1 eLine1)
        let sTextLine2 = textLines !! (sLine2 - 1)
        sCol2 <- forAll . Gen.int $ case () of
          _ | eLine1 == sLine1 && sLine2 == sLine1 -> Range.linear sCol1 eCol1
          _ | sLine2 == sLine1 -> Range.linear sCol1 (Text.length sTextLine2)
          _ | sLine2 == eLine1 -> Range.linear 1 eCol1
          _ -> Range.linear 1 (Text.length sTextLine2)

        eLine2 <- forAll $ Gen.int (Range.linear sLine2 (length textLines))
        let eTextLine2 = textLines !! (eLine2 - 1)
        eCol2 <- forAll $ Gen.int (Range.linear (if eLine2 == sLine2 then sCol2 else 1) (Text.length eTextLine2))

        let replacements =
              [ (CMarkGFM.PosInfo sLine1 sCol1 eLine1 eCol1, "FOO"),
                (CMarkGFM.PosInfo sLine2 sCol2 eLine2 eCol2, "BAR")
              ]
        replaceSlices text replacements === Nothing

    context "when given non-positive start line" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        sLine <- forAll $ Gen.int (Range.linear minBound 0)
        sCol <- forAll $ Gen.int (Range.linear 1 (minimum $ map Text.length textLines))

        eLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear 1 (Text.length eTextLine))

        replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing

    context "when given non-positive start col" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        sCol <- forAll $ Gen.int (Range.linear minBound 0)

        eLine <- forAll $ Gen.int (Range.linear sLine (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear 1 (Text.length eTextLine))

        replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing

    context "when given non-positive end line" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear minBound 0)
        eCol <- forAll $ Gen.int (Range.linear sCol (minimum $ map Text.length textLines))

        replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing

    context "when given non-positive end col" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear sLine (length textLines))
        eCol <- forAll $ Gen.int (Range.linear minBound 0)

        replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing

    context "when given out-of-range start line" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        sLine <- forAll $ Gen.int (Range.linear (length textLines + 1) maxBound)
        sCol <- forAll $ Gen.int (Range.linear 1 (minimum $ map Text.length textLines))

        eLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear 1 (Text.length eTextLine))

        replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing

    context "when given out-of-range start col" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear (Text.length sTextLine + 1) maxBound)

        eLine <- forAll $ Gen.int (Range.linear sLine (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear 1 (Text.length eTextLine))

        replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing

    context "when given out-of-range end line" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear (length textLines + 1) maxBound)
        eCol <- forAll $ Gen.int (Range.linear sCol (minimum $ map Text.length textLines))

        replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing

    context "when given out-of-range end col" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        sLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <- forAll $ Gen.int (Range.linear 1 (Text.length sTextLine))

        eLine <- forAll $ Gen.int (Range.linear sLine (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear (Text.length eTextLine + 1) maxBound)

        replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing

    context "when given start indices that are after the end indices" $ do
      it "returns Nothing" $ do
        textLines <- forAll genTextLines
        let text = Text.intercalate "\n" textLines

        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        eLine <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let eTextLine = textLines !! (eLine - 1)
        eCol <- forAll $ Gen.int (Range.linear (Text.length eTextLine) maxBound)

        sLine <- forAll $ Gen.int (Range.linear eLine (length textLines))
        let sTextLine = textLines !! (sLine - 1)
        sCol <-
          forAll . Gen.int $
            if sLine == eLine
              then
                Range.linear eCol (Text.length sTextLine)
              else
                Range.linear 1 (Text.length sTextLine)

        if sLine == eLine && sCol == eCol
          then
            -- It's still possible to generate a valid set of start and end indices with the generators above,
            -- so we skip the check in that case.
            success
          else
            replaceSlices text [(CMarkGFM.PosInfo sLine sCol eLine eCol, repl)] === Nothing
