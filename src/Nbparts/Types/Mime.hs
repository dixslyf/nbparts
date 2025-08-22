module Nbparts.Types.Mime
  ( UnembeddedMimeAttachments (..),
    UnembeddedMimeBundle (..),
    UnembeddedMimeData (..),
  )
where

import Data.Aeson (Options (constructorTagModifier, sumEncoding))
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype UnembeddedMimeAttachments = UnembeddedMimeAttachments (Map Text UnembeddedMimeBundle)
  deriving (Generic, Show, Eq, Ord)

newtype UnembeddedMimeBundle = UnembeddedMimeBundle (Map Text UnembeddedMimeData)
  deriving (Generic, Show, Eq, Ord)

data UnembeddedMimeData = BinaryData FilePath | TextualData Text | JsonData Aeson.Value
  deriving (Generic, Show, Eq, Ord)

instance Aeson.ToJSON UnembeddedMimeAttachments

instance Aeson.FromJSON UnembeddedMimeAttachments

instance Aeson.ToJSON UnembeddedMimeBundle

instance Aeson.FromJSON UnembeddedMimeBundle

instance Aeson.ToJSON UnembeddedMimeData where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON UnembeddedMimeData where
  parseJSON = Aeson.genericParseJSON jsonOptions

jsonOptions :: Aeson.Options
jsonOptions =
  Aeson.defaultOptions
    { sumEncoding =
        Aeson.TaggedObject
          { tagFieldName = "type",
            contentsFieldName = "value"
          },
      constructorTagModifier = \case
        "BinaryData" -> "binary"
        "TextualData" -> "text"
        "JsonData" -> "json"
        other -> other
    }
