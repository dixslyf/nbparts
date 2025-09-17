module Tests.Util.Json where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (Scientific, scientific)
import Data.Vector qualified as Vector
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genValue :: Gen Aeson.Value
genValue =
  Gen.recursive Gen.choice nonRecursive recursive
  where
    nonRecursive :: [Gen Aeson.Value]
    nonRecursive =
      [ pure Aeson.Null,
        Aeson.Bool <$> Gen.bool,
        Aeson.String <$> Gen.text (Range.linear 1 10) Gen.unicode,
        Aeson.Number <$> genScientific
      ]

    recursive :: [Gen Aeson.Value]
    recursive =
      [ genArrayValue,
        genObjectValue
      ]

    genArrayValue :: Gen Aeson.Value
    genArrayValue = do
      values <- Gen.list (Range.linear 0 5) genValue
      pure $ Aeson.Array (Vector.fromList values)

    genObjectValue :: Gen Aeson.Value
    genObjectValue = do
      obj <- Gen.map (Range.linear 0 5) $ (,) <$> Gen.text (Range.linear 10 20) Gen.unicode <*> genValue
      pure $ Aeson.Object (KeyMap.fromMapText obj)

genScientific :: Gen Scientific
genScientific = do
  c <- Gen.integral (Range.linear (-1000000) 1000000)
  e <- Gen.int (Range.linear (-6) 6)
  pure $ scientific c e
