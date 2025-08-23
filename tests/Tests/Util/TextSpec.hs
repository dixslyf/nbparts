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
      it "returns Nothing" $ do
        let textLines = []
        lineColToIndex textLines 0 0 `shouldBe` Nothing
        lineColToIndex textLines 1 1 `shouldBe` Nothing
        lineColToIndex textLines 3 3 `shouldBe` Nothing

    context "when given a simple case" $ do
      it "computes the correct index" $ do
        -- "abc\ndefg\nhijkl"
        let textLines = ["abc", "defg", "hijkl"]
        lineColToIndex textLines 1 1 `shouldBe` Just 0
        lineColToIndex textLines 1 2 `shouldBe` Just 1
        lineColToIndex textLines 2 1 `shouldBe` Just 4
        lineColToIndex textLines 3 3 `shouldBe` Just 11

    it "matches indexing into the joined text" $ hedgehog $ do
      txtLines <- forAll $ Gen.list (Range.linear 1 10) (Gen.text (Range.linear 1 100) Gen.unicode)

      line <- forAll $ Gen.int (Range.linear 1 (length txtLines))
      let txtLine = txtLines !! (line - 1)

      col <- forAll $ Gen.int (Range.linear 1 (Text.length txtLine))

      let joined = Text.intercalate "\n" txtLines
          idx = lineColToIndex txtLines line col
      (Text.index joined <$> idx) === Just (Text.index txtLine (col - 1))

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
