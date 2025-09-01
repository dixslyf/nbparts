module Tests.Types.ManifestSpec where

import Control.Arrow (left)
import Data.Aeson qualified as Aeson
import Data.Version (Version (Version))
import Data.Yaml qualified as Yaml
import Hedgehog (Gen, forAll, tripping, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Types.Manifest
  ( Format (FormatJson, FormatMarkdown, FormatYaml),
    Manifest
      ( Manifest,
        metadataFormat,
        nbpartsVersion,
        outputsFormat,
        sourcesFormat
      ),
    currentNbpartsVersion,
    defManifest,
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

genVersion :: Gen Version
genVersion = do
  branches <- Gen.list (Range.linear 3 4) (Gen.int $ Range.linear 0 10)
  pure $ Version branches []

genFormat :: Gen Format
genFormat = Gen.element [FormatYaml, FormatJson, FormatMarkdown]

genSourcesFormat :: Gen Format
genSourcesFormat = genFormat

genMetadataFormat :: Gen Format
genMetadataFormat = Gen.element [FormatYaml, FormatJson]

genOutputsFormat :: Gen Format
genOutputsFormat = Gen.element [FormatYaml, FormatJson]

genManifest :: Gen Manifest
genManifest = do
  nbpartsVersion <- genVersion
  sourcesFormat <- genSourcesFormat
  metadataFormat <- genMetadataFormat
  outputsFormat <- genOutputsFormat
  pure $
    Manifest
      { nbpartsVersion,
        sourcesFormat,
        metadataFormat,
        outputsFormat
      }

spec :: Spec
spec = do
  describe "Manifest" $ do
    it "JSON roundtrip" $ hedgehog $ do
      manifest <- forAll genManifest
      tripping manifest Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      manifest <- forAll genManifest
      tripping manifest Yaml.encode (left (const ()) . Yaml.decodeEither')

    it "defManifest has correct nbpartsVersion" $ hedgehog $ do
      let Manifest {nbpartsVersion} = defManifest
      nbpartsVersion === currentNbpartsVersion

  describe "Format" $ do
    it "JSON roundtrip" $ hedgehog $ do
      fmt <- forAll genFormat
      tripping fmt Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      fmt <- forAll genFormat
      tripping fmt Yaml.encode (left (const ()) . Yaml.decodeEither')
