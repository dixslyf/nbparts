module Tests.Types.MimeSpec where

import Control.Arrow (left)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text.Encoding qualified as Text
import Data.Yaml qualified as Yaml
import Hedgehog (Gen, forAll, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Types.Mime
  ( UnembeddedMimeAttachments (UnembeddedMimeAttachments),
    UnembeddedMimeBundle (UnembeddedMimeBundle),
    UnembeddedMimeData (BinaryData, JsonData, TextualData),
  )
import Network.Mime (MimeType)
import Network.Mime qualified as Mime
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Tests.Util.Json (genValue)

genUnembeddedMimeData :: Gen UnembeddedMimeData
genUnembeddedMimeData =
  Gen.choice
    [ BinaryData <$> Gen.string (Range.linear 1 50) Gen.unicode,
      TextualData <$> Gen.text (Range.linear 1 500) Gen.unicode,
      JsonData <$> genValue
    ]

genMimeType :: Gen MimeType
genMimeType = Gen.choice $ map Gen.constant (Map.elems Mime.defaultMimeMap)

genUnembeddedMimeBundle :: Gen UnembeddedMimeBundle
genUnembeddedMimeBundle =
  UnembeddedMimeBundle <$> Gen.map (Range.linear 0 5) ((,) <$> mt <*> genUnembeddedMimeData)
  where
    mt = Text.decodeUtf8 <$> genMimeType

genUnembeddedMimeAttachments :: Gen UnembeddedMimeAttachments
genUnembeddedMimeAttachments =
  UnembeddedMimeAttachments
    <$> Gen.map
      (Range.linear 0 5)
      ( (,)
          <$> Gen.text (Range.linear 1 500) Gen.unicode
          <*> genUnembeddedMimeBundle
      )

spec :: Spec
spec = do
  describe "UnembeddedMimeData" $ do
    it "JSON roundtrip" $ hedgehog $ do
      d <- forAll genUnembeddedMimeData
      tripping d Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      d <- forAll genUnembeddedMimeData
      tripping d Yaml.encode (left (const ()) . Yaml.decodeEither')

  describe "UnembeddedMimeBundle" $ do
    it "JSON roundtrip" $ hedgehog $ do
      b <- forAll genUnembeddedMimeBundle
      tripping b Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      b <- forAll genUnembeddedMimeBundle
      tripping b Yaml.encode (left (const ()) . Yaml.decodeEither')

  describe "UnembeddedMimeAttachments" $ do
    it "JSON roundtrip" $ hedgehog $ do
      a <- forAll genUnembeddedMimeAttachments
      tripping a Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      a <- forAll genUnembeddedMimeAttachments
      tripping a Yaml.encode (left (const ()) . Yaml.decodeEither')
