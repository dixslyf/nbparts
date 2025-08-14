module Nbparts.Pack.Error where

import Text.Pandoc (PandocError)

newtype PackError = PackPandocError PandocError
