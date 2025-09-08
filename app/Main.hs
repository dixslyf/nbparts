module Main where

import Control.Monad.Except (runExceptT)
import Data.Text.IO qualified as TIO
import Nbparts.Cli.Options (parseOpts)
import Nbparts.Run qualified as Nbparts
import Nbparts.Types (NbpartsError, renderError)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

main :: IO ()
main = do
  opts <- parseOpts
  result <- runExceptT $ Nbparts.run opts
  case result of
    Right _ -> pure ()
    Left err -> exitError err

exitError :: NbpartsError -> IO a
exitError err = do
  TIO.hPutStrLn stderr $ renderError err
  exitWith $ ExitFailure 1
