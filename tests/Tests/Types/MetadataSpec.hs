module Tests.Types.MetadataSpec where

import Control.Arrow (left)
import Data.Aeson qualified as Aeson
import Data.Ipynb qualified as Ipynb
import Data.Registry.Hedgehog.AesonGenerators (genValue)
import Data.Yaml qualified as Yaml
import Hedgehog (Gen, forAll, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Types.Metadata (CellMetadata (CodeCellMetadata, GenericCellMetadata), NotebookMetadata (NotebookMetadata))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Tests.Types.SourcesSpec (genCellId)

genJSONMeta :: Gen Ipynb.JSONMeta
genJSONMeta = do
  kvs <- Gen.map (Range.linear 0 5) $ do
    k <- Gen.text (Range.linear 1 10) Gen.alphaNum
    v <- genValue
    pure (k, v)
  pure $ Ipynb.JSONMeta kvs

genCellMetadata :: Gen CellMetadata
genCellMetadata =
  Gen.choice
    [ CodeCellMetadata
        <$> Gen.maybe (Gen.int $ Range.linear 0 100)
        <*> genJSONMeta,
      GenericCellMetadata <$> genJSONMeta
    ]

genNotebookMetadata :: Gen NotebookMetadata
genNotebookMetadata = do
  major <- Gen.int $ Range.linear 1 5
  minor <- Gen.int $ Range.linear 0 9

  toplevel <- genJSONMeta

  cells <- Gen.map (Range.linear 0 10) $ do
    cellId <- genCellId
    v <- genCellMetadata
    pure (cellId, v)

  pure $ NotebookMetadata major minor toplevel cells

spec :: Spec
spec = do
  describe "NotebookMetadata" $ do
    it "JSON roundtrip" $ hedgehog $ do
      nm <- forAll genNotebookMetadata
      tripping nm Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      nm <- forAll genNotebookMetadata
      tripping nm Yaml.encode (left (const ()) . Yaml.decodeEither')

  describe "CellMetadata" $ do
    it "JSON roundtrip" $ hedgehog $ do
      cm <- forAll genCellMetadata
      tripping cm Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      cm <- forAll genCellMetadata
      tripping cm Yaml.encode (left (const ()) . Yaml.decodeEither')
