module Nbparts.Util.Map where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

lookupByKeyPrefix :: Text -> Map Text v -> Maybe v
lookupByKeyPrefix prefix m =
  -- First entry after performing `dropWhile` is the first entry whose key starts with the prefix.
  case dropWhile (not . Text.isPrefixOf prefix . fst) entries of
    ((_key, value) : _) -> Just value
    [] -> Nothing
  where
    entries = Map.toList m
