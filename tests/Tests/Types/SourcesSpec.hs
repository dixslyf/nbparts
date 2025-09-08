module Tests.Types.SourcesSpec where

import Control.Arrow (left)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Hedgehog (Gen, forAll, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nbparts.Types.Sources
  ( CellMarker (CellMarker),
    CellSource (CellSource),
    CellType
      ( Code,
        Markdown,
        Raw
      ),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Tests.Types.MimeSpec (genUnembeddedMimeAttachments)

genCellId :: Gen Text
genCellId = Gen.text (Range.linear 16 64) Gen.unicode

genCellType :: Gen CellType
genCellType = Gen.element [Markdown, Raw, Code]

genSentence :: Gen Text
genSentence =
  Gen.text (Range.linear 20 50) Gen.alphaNum

genMarkdownSnippet :: Gen Text
genMarkdownSnippet =
  Gen.choice
    [ ("# " <>) <$> genSentence,
      ("## " <>) <$> genSentence,
      ("### " <>) <$> genSentence,
      ("- " <>) <$> genSentence,
      (\s -> "*" <> s <> "*") <$> genSentence,
      (\s -> "**" <> s <> "**") <$> genSentence,
      (\s -> "_" <> s <> "_") <$> genSentence,
      (\s -> "```\n" <> s <> "\n```") <$> genSentence,
      (\title url -> "[" <> title <> "](" <> url <> ")") <$> genSentence <*> genSentence,
      (\title url -> "![" <> title <> "](" <> url <> ")") <$> genSentence <*> genSentence,
      genSentence,
      pure "\n"
    ]

genMarkdownDoc :: Gen Text
genMarkdownDoc = do
  blocks <- Gen.list (Range.linear 1 5) genMarkdownSnippet
  pure (Text.unlines blocks)

genCellSource :: Gen CellSource
genCellSource =
  CellSource
    <$> genCellId
    <*> genCellType
    <*> (Text.lines <$> genMarkdownDoc)
    <*> Gen.maybe genUnembeddedMimeAttachments

genCellMarker :: Gen CellMarker
genCellMarker =
  CellMarker
    <$> genCellId
    <*> genCellType
    <*> Gen.maybe genUnembeddedMimeAttachments

spec :: Spec
spec = do
  describe "CellType" $ do
    it "JSON roundtrip" $ hedgehog $ do
      ct <- forAll genCellType
      tripping ct Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      ct <- forAll genCellType
      tripping ct Yaml.encode (left (const ()) . Yaml.decodeEither')

  describe "CellSource" $ do
    it "JSON roundtrip" $ hedgehog $ do
      cs <- forAll genCellSource
      tripping cs Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      cs <- forAll genCellSource
      tripping cs Yaml.encode (left (const ()) . Yaml.decodeEither')

  describe "CellMarker" $ do
    it "JSON roundtrip" $ hedgehog $ do
      cm <- forAll genCellMarker
      tripping cm Aeson.encode Aeson.decode

    it "YAML roundtrip" $ hedgehog $ do
      cm <- forAll genCellMarker
      tripping cm Yaml.encode (left (const ()) . Yaml.decodeEither')
