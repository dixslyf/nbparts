{-# LANGUAGE UndecidableInstances #-}

module Nbparts.Types.Outputs
  ( NotebookOutputs (..),
    UnembeddedNotebookOutputs (..),
    UnembeddedCellOutput (..),
  )
where

import Data.Aeson (Options (constructorTagModifier, sumEncoding))
import Data.Aeson qualified as Aeson
import Data.Ipynb qualified as Ipynb
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Nbparts.Types.Mime (UnembeddedMimeBundle)

newtype NotebookOutputs a = NotebookOutputs (Map Text [Ipynb.Output a]) -- Map of Cell IDs to outputs.
  deriving (Generic, Show)

newtype UnembeddedNotebookOutputs = UnembeddedNotebookOutputs (Map Text [UnembeddedCellOutput])
  deriving (Generic, Show)

data UnembeddedCellOutput
  = Stream
      { name :: Text,
        lines :: [Text]
      }
  | DisplayData
      { outputs :: UnembeddedMimeBundle,
        metadata :: Ipynb.JSONMeta
      }
  | ExecuteResult
      { executeCount :: Int,
        outputs :: UnembeddedMimeBundle,
        metadata :: Ipynb.JSONMeta
      }
  | Err
      { name :: Text,
        value :: Text,
        traceback :: [Text]
      }
  deriving (Generic, Show)

instance (Aeson.ToJSON (Ipynb.Output a)) => Aeson.ToJSON (NotebookOutputs a)

instance (Aeson.FromJSON (Ipynb.Output a)) => Aeson.FromJSON (NotebookOutputs a)

instance Aeson.ToJSON UnembeddedNotebookOutputs

instance Aeson.FromJSON UnembeddedNotebookOutputs

instance Aeson.ToJSON UnembeddedCellOutput where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON UnembeddedCellOutput where
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
        "Stream" -> "stream"
        "DisplayData" -> "display-data"
        "ExecuteResult" -> "execute-result"
        "Err" -> "error"
        other -> other
    }
