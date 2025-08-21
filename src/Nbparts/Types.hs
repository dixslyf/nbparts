-- Since this module mostly re-exports the submodules,
-- we disable the warning.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Nbparts.Types
  ( module Nbparts.Types.Manifest,
    module Nbparts.Types.Sources,
    module Nbparts.Types.Outputs,
    module Nbparts.Types.Metadata,
    module Nbparts.Types.Mime,
    module Nbparts.Types.Error,
    SomeNotebook (..),
    withSomeNotebook,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson qualified as Aeson
import Data.Ipynb qualified as Ipynb
import Nbparts.Types.Error
import Nbparts.Types.Manifest
import Nbparts.Types.Metadata
import Nbparts.Types.Mime
import Nbparts.Types.Outputs
import Nbparts.Types.Sources

data SomeNotebook where
  SomeNotebook :: (Aeson.ToJSON (Ipynb.Notebook a), Aeson.FromJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> SomeNotebook

withSomeNotebook :: SomeNotebook -> (forall a. (Aeson.ToJSON (Ipynb.Notebook a), Aeson.FromJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> r) -> r
withSomeNotebook (SomeNotebook nb) f = f nb

instance Show SomeNotebook where
  show (SomeNotebook nb) = "SomeNotebook (" <> show nb <> ")"

instance Aeson.ToJSON SomeNotebook where
  toJSON (SomeNotebook nb) = Aeson.toJSON nb

instance Aeson.FromJSON SomeNotebook where
  parseJSON v =
    SomeNotebook <$> (Aeson.parseJSON @(Ipynb.Notebook Ipynb.NbV3)) v
      <|> SomeNotebook <$> (Aeson.parseJSON @(Ipynb.Notebook Ipynb.NbV4)) v
