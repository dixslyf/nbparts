module Tests.Util.MapSpec where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Util.Map (lookupByKeyPrefix)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "lookupByKeyPrefix" $ do
    it "finds exact matches" $ do
      let m :: Map Text Int
          m = Map.fromList [("apple", 1), ("banana", 2)]
      lookupByKeyPrefix "apple" m `shouldBe` Just 1
      lookupByKeyPrefix "banana" m `shouldBe` Just 2

    it "finds by prefix" $ do
      let m :: Map Text Int
          m = Map.fromList [("apple", 1), ("banana", 2)]
      lookupByKeyPrefix "app" m `shouldBe` Just 1
      lookupByKeyPrefix "ban" m `shouldBe` Just 2

    context "when there are multiple keys with the prefix" $ do
      it "returns the first matching key" $ do
        let m :: Map Text Int
            m = Map.fromList [("app", 1), ("apple", 2), ("apples", 3)]
        lookupByKeyPrefix "ap" m `shouldBe` Just 1
        lookupByKeyPrefix "app" m `shouldBe` Just 1
        lookupByKeyPrefix "appl" m `shouldBe` Just 2
        lookupByKeyPrefix "apple" m `shouldBe` Just 2
        lookupByKeyPrefix "apples" m `shouldBe` Just 3

    context "when no key has the prefix" $ do
      it "returns Nothing" $ do
        let m :: Map Text Int
            m = Map.fromList [("apple", 1), ("banana", 2)]
        lookupByKeyPrefix "carrot" m `shouldBe` Nothing

    context "when the map is empty" $ do
      it "returns Nothing" $ hedgehog $ do
        prefix <- forAll $ Gen.text (Range.linear 0 5) Gen.alphaNum
        lookupByKeyPrefix prefix (Map.empty :: Map Text Int) === Nothing

    it "is consistent with filtering + minimum" $ hedgehog $ do
      prefix <- forAll $ Gen.text (Range.linear 0 5) Gen.alphaNum
      keys <- forAll $ Gen.list (Range.linear 0 10) (Gen.text (Range.linear 1 8) Gen.alphaNum)
      vals <- forAll $ Gen.list (Range.singleton (length keys)) (Gen.int (Range.linear 0 100))
      let m = Map.fromList (zip keys vals)
      let expected =
            case [(k, v) | (k, v) <- Map.toList m, Text.isPrefixOf prefix k] of
              (_, v) : _ -> Just v
              [] -> Nothing
      lookupByKeyPrefix prefix m === expected
