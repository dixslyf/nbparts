module Nbparts.Util.Prompt where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.IO (hFlush, stdout)

confirm :: Text -> IO Bool
confirm prompt = do
  Text.putStr $ prompt <> " (Y/n) "
  hFlush stdout
  res <- Text.getLine
  case Text.toLower res of
    "y" -> pure True
    _
      | Text.null res -> pure True
      | otherwise -> pure False
