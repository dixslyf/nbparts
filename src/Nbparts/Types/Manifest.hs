module Nbparts.Types.Manifest
  ( currentNbpartsVersion,
    Manifest (..),
    Format (..),
    mkManifest,
  )
where

import Data.Aeson (Options (constructorTagModifier))
import Data.Aeson qualified as Aeson
import Data.Version (Version)
import GHC.Generics (Generic)
import Paths_nbparts qualified

currentNbpartsVersion :: Version
currentNbpartsVersion = Paths_nbparts.version

data Manifest = Manifest
  { nbpartsVersion :: Version,
    sourcesFormat :: Format
  }
  deriving (Generic, Show, Eq, Ord)

mkManifest :: Format -> Manifest
mkManifest = Manifest currentNbpartsVersion

data Format = FormatYaml | FormatMarkdown
  deriving (Generic, Show, Eq, Ord)

instance Aeson.ToJSON Manifest where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON Manifest where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance Aeson.ToJSON Format where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON Format where
  parseJSON = Aeson.genericParseJSON jsonOptions

jsonOptions :: Aeson.Options
jsonOptions =
  Aeson.defaultOptions
    { constructorTagModifier = \case
        "FormatYaml" -> "yaml"
        "FormatMarkdown" -> "markdown"
        other -> other
    }
