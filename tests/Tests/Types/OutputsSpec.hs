module Tests.Types.OutputsSpec where

import Control.Arrow (left)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Yaml qualified as Yaml
import Hedgehog (Gen, forAll, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Types.Outputs
  ( UnembeddedCellOutput (DisplayData, Err, ExecuteResult, Stream),
    UnembeddedNotebookOutputs (UnembeddedNotebookOutputs),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Tests.Types.MetadataSpec (genJSONMeta)
import Tests.Types.MimeSpec (genUnembeddedMimeBundle)
import Tests.Types.SourcesSpec (genCellId)

genStreamName :: Gen Text
genStreamName = Gen.element ["stdout", "stderr"]

genUnembeddedCellOutput :: Gen UnembeddedCellOutput
genUnembeddedCellOutput =
  Gen.choice
    [ Stream <$> genStreamName <*> Gen.list (Range.linear 0 5) (Gen.text (Range.linear 1 100) Gen.unicode),
      DisplayData <$> genUnembeddedMimeBundle <*> genJSONMeta,
      ExecuteResult <$> Gen.int (Range.linear 0 100) <*> genUnembeddedMimeBundle <*> genJSONMeta,
      Err
        <$> Gen.text (Range.linear 10 20) Gen.unicode
        <*> Gen.text (Range.linear 30 50) Gen.unicode
        <*> Gen.list (Range.linear 0 5) (Gen.text (Range.linear 30 50) Gen.unicode)
    ]

genUnembeddedNotebookOutputs :: Gen UnembeddedNotebookOutputs
genUnembeddedNotebookOutputs =
  UnembeddedNotebookOutputs
    <$> Gen.map
      (Range.linear 0 3)
      ((,) <$> genCellId <*> Gen.list (Range.linear 0 3) genUnembeddedCellOutput)

spec :: Spec
spec = do
  describe "UnembeddedCellOutput" $ do
    it "JSON roundtrip" $ hedgehog $ do
      out <- forAll genUnembeddedCellOutput
      tripping out Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      out <- forAll genUnembeddedCellOutput
      tripping out Yaml.encode (left (const ()) . Yaml.decodeEither')

  describe "UnembeddedNotebookOutputs" $ do
    it "JSON roundtrip" $ hedgehog $ do
      outs <- forAll genUnembeddedNotebookOutputs
      tripping outs Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      outs <- forAll genUnembeddedNotebookOutputs
      tripping outs Yaml.encode (left (const ()) . Yaml.decodeEither')
