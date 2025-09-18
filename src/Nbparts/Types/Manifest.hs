module Nbparts.Types.Manifest
  ( currentNbpartsVersion,
    Manifest (..),
    Format (..),
    defManifest,
    formatExtension,
  )
where

import Data.Aeson (Options (constructorTagModifier))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Version (Version)
import GHC.Generics (Generic)
import Paths_nbparts qualified

currentNbpartsVersion :: Version
currentNbpartsVersion = Paths_nbparts.version

data Manifest = Manifest
  { nbpartsVersion :: Version,
    sourcesFormat :: Format,
    metadataFormat :: Format,
    outputsFormat :: Format
  }
  deriving (Generic, Show, Eq, Ord)

defManifest :: Manifest
defManifest =
  Manifest
    { nbpartsVersion = currentNbpartsVersion,
      sourcesFormat = FormatYaml,
      metadataFormat = FormatYaml,
      outputsFormat = FormatYaml
    }

data Format
  = FormatYaml
  | FormatJson
  | FormatMarkdown
  deriving (Generic, Show, Eq, Ord)

formatExtension :: Format -> String
formatExtension FormatYaml = "yaml"
formatExtension FormatJson = "json"
formatExtension FormatMarkdown = "md"

instance Aeson.ToJSON Manifest

instance Aeson.FromJSON Manifest

instance Aeson.ToJSON Format where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON Format where
  parseJSON = Aeson.withText "Format" $ \case
    "yaml" -> pure FormatYaml
    "json" -> pure FormatJson
    "markdown" -> pure FormatMarkdown
    -- Backward compatibility. v0.1.0.0 mistakenly serialised `FormatJson` as
    -- "FormatJson" instead of "json", so now we're stuck with dealing with that.
    -- For symmetry's sake, might as well include the rest of the formats as well.
    "FormatYaml" -> pure FormatYaml
    "FormatJson" -> pure FormatJson
    "FormatMarkdown" -> pure FormatMarkdown
    other -> fail $ "Unknown format: " <> Text.unpack other

jsonOptions :: Aeson.Options
jsonOptions =
  Aeson.defaultOptions
    { constructorTagModifier = \case
        "FormatYaml" -> "yaml"
        "FormatJson" -> "json"
        "FormatMarkdown" -> "markdown"
        other -> other
    }
