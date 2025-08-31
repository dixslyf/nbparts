module Tests.Util.TextSpec where

import Data.Text qualified as Text
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Util.Text (lineColToIndex, replaceSlices, splitKeepNewlines)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "lineColToIndex" $ do
    context "when given an empty list" $ do
      it "returns Nothing" $ hedgehog $ do
        let textLines = []
        line <- forAll $ Gen.int (Range.linear minBound maxBound)
        col <- forAll $ Gen.int (Range.linear minBound maxBound)
        lineColToIndex textLines line col === Nothing

    context "when given a single line with valid line and col" $ do
      it "returns `col - 1`" $ do
        textLine <- forAll $ Gen.text (Range.linear 1 50) Gen.unicode
        let textLines = [textLine]
        col <- forAll $ Gen.int (Range.linear 1 (Text.length textLine + 1))
        lineColToIndex textLines 1 col === Just (col - 1)

    context "when given multiple lines with valid line and col" $ do
      it "computes the correct index for a simple case" $ do
        -- "abc\ndefg\nhijkl"
        let textLines = ["abc", "defg", "hijkl"]
        lineColToIndex textLines 1 1 `shouldBe` Just 0
        lineColToIndex textLines 1 2 `shouldBe` Just 1
        lineColToIndex textLines 2 1 `shouldBe` Just 4
        lineColToIndex textLines 3 3 `shouldBe` Just 11
        lineColToIndex textLines 3 6 `shouldBe` Just 14

      it "returns an index whose letter is equal to the one retrieved using the line-col index" $ hedgehog $ do
        textLines <-
          forAll $
            Gen.list
              (Range.linear 1 5)
              (Gen.text (Range.linear 1 50) Gen.unicode)

        line <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let textLine = textLines !! (line - 1)

        col <- forAll $ Gen.int (Range.linear 1 (Text.length textLine))
        let letter = Text.index textLine (col - 1)

        let text = Text.intercalate "\n" textLines
        let maybeIdx = lineColToIndex textLines line col
        let letter' = Text.index text <$> maybeIdx

        Just letter === letter'

    context "when given non-positive indices" $ do
      it "returns Nothing" $ hedgehog $ do
        textLines <-
          forAll $
            Gen.list
              (Range.linear 1 5)
              (Gen.text (Range.linear 1 50) Gen.unicode)
        line <- forAll $ Gen.int (Range.linear (-100) 0)
        col <- forAll $ Gen.int (Range.linear (-100) 0)
        lineColToIndex textLines line col === Nothing

    context "when given out-of-range line" $ do
      it "returns Nothing" $ hedgehog $ do
        textLines <-
          forAll $
            Gen.list
              (Range.linear 1 5)
              (Gen.text (Range.linear 1 50) Gen.unicode)
        line <- forAll $ Gen.int (Range.linear (length textLines + 1) 1000)
        lineColToIndex textLines line 1 === Nothing

    context "when given out-of-range col" $ do
      it "returns Nothing" $ hedgehog $ do
        textLines <-
          forAll $
            Gen.list
              (Range.linear 1 5)
              (Gen.text (Range.linear 1 50) Gen.unicode)
        line <- forAll $ Gen.int (Range.linear 1 (length textLines))
        let textLine = textLines !! (line - 1)
        col <- forAll $ Gen.int (Range.linear (Text.length textLine + 2) 1000)
        lineColToIndex textLines line col === Nothing

  describe "replaceSlices" $ do
    context "when given no replacements" $ do
      it "is identity" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 0 250) Gen.unicode
        replaceSlices t [] === Just t

    context "when given only one replacement" $ do
      it "behaves like manual splice" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 1 100) Gen.unicode
        start <- forAll $ Gen.int (Range.linear 0 (Text.length t))
        end <- forAll $ Gen.int (Range.linear start (Text.length t))
        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        let manual = Text.take start t <> repl <> Text.drop end t
            replaced = replaceSlices t [((start, end), repl)]
        replaced === Just manual

    context "when given only one replacement at the start" $ do
      it "behaves like prepending" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 1 100) Gen.unicode
        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode

        let replaced = replaceSlices t [((0, 0), repl)]
        replaced === Just (repl <> t)

    context "when given only one replacement at the end" $ do
      it "behaves like appending" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 1 100) Gen.unicode
        repl <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode
        let len = Text.length t

        let replaced = replaceSlices t [((len, len), repl)]
        replaced === Just (t <> repl)

    context "when given non-overlapping slices" $ do
      it "correctly replaces" $ do
        replaceSlices "abcdef" [((1, 3), "XYZ")] `shouldBe` Just "aXYZdef"
        replaceSlices "abcdef" [((0, 1), "XYZ"), ((3, 6), "FOOBAR")] `shouldBe` Just "XYZbcFOOBAR"
        replaceSlices "abcdef" [((0, 1), "XYZ"), ((1, 3), "FOO"), ((3, 6), "BAR")] `shouldBe` Just "XYZFOOBAR"

    context "when given overlapping slices" $ do
      it "returns Nothing" $ do
        replaceSlices "abcdef" [((0, 4), "FOO"), ((1, 5), "BAR")] `shouldBe` Nothing

    context "when given negative indices" $ do
      it "returns Nothing" $ do
        replaceSlices "abcdef" [((-1, 3), "FOO")] `shouldBe` Nothing
        replaceSlices "abcdef" [((1, -1), "FOO")] `shouldBe` Nothing
        replaceSlices "abcdef" [((-3, -1), "FOO")] `shouldBe` Nothing

    context "when given out-of-range indices" $ do
      it "returns Nothing" $ do
        replaceSlices "abcdef" [((6, 8), "FOO")] `shouldBe` Nothing
        replaceSlices "abcdef" [((3, 8), "FOO")] `shouldBe` Nothing

  describe "splitKeepNewlines" $ do
    context "when given empty text" $ do
      it "returns an empty list" $ do
        splitKeepNewlines "" `shouldBe` []

    context "when given text intercalated with single newlines" $ do
      it "splits and keeps newlines" $ do
        splitKeepNewlines "abc\ndefg\nhijkl" `shouldBe` ["abc\n", "defg\n", "hijkl"]

    context "when given text intercalated with multiple newlines" $ do
      it "splits and keeps newlines" $ do
        splitKeepNewlines "abc\n\ndefg\n\n\nhijkl" `shouldBe` ["abc\n", "\n", "defg\n", "\n", "\n", "hijkl"]

    context "when given text with trailing newlines" $ do
      it "splits and keeps newlines" $ do
        splitKeepNewlines "abc\ndefg\n\nhijkl\n" `shouldBe` ["abc\n", "defg\n", "\n", "hijkl\n"]
        splitKeepNewlines "abc\ndefg\n\nhijkl\n\n\n" `shouldBe` ["abc\n", "defg\n", "\n", "hijkl\n", "\n", "\n"]

    it "concatenation of parts is the original text" $ hedgehog $ do
      ls <-
        forAll $
          Gen.list
            (Range.linear 0 10)
            (Gen.text (Range.linear 0 100) Gen.unicode)
      let txt = Text.intercalate "\n" ls
      mconcat (splitKeepNewlines txt) === txt
