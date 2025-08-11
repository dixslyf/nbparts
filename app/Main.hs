{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative qualified as OA
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)
import Text.Pandoc qualified as Pandoc
import Unpack

newtype AppOptions = AppOptions
  { command :: Command
  }

newtype UnpackOptions = UnpackOptions
  { notebook :: FilePath
  }

newtype PackOptions = PackOptions
  { directory :: FilePath
  }

data Command = Unpack UnpackOptions | Pack PackOptions

unpackOptionsParser :: OA.Parser UnpackOptions
unpackOptionsParser = UnpackOptions <$> OA.argument OA.str (OA.metavar "NOTEBOOK" <> OA.help "Path to the notebook to unpack")

packOptionsParser :: OA.Parser PackOptions
packOptionsParser = PackOptions <$> OA.argument OA.str (OA.metavar "DIRECTORY" <> OA.help "Path to the directory to pack into a notebook")

commandParser :: OA.Parser Command
commandParser =
  OA.hsubparser $
    OA.command "unpack" (OA.info (Unpack <$> unpackOptionsParser) (OA.progDesc "Unpack a notebook"))
      <> OA.command "pack" (OA.info (Pack <$> packOptionsParser) (OA.progDesc "Pack a directory into a notebook"))

appOptionsParser :: OA.Parser AppOptions
appOptionsParser = AppOptions <$> commandParser

parseOpts :: IO AppOptions
parseOpts = OA.customExecParser prefs opts
  where
    prefs = OA.prefs OA.showHelpOnEmpty
    opts = OA.info (OA.helper <*> appOptionsParser) (OA.fullDesc <> OA.progDesc "Unpack a Jupyter notebook into its content, metadata and outputs")

newtype NbpartsError = UnpackError UnpackError

main :: IO ()
main = do
  opts <- parseOpts
  case command opts of
    Unpack (UnpackOptions notebook) -> do
      result <- unpack notebook
      case result of
        Right _ -> pure ()
        Left err -> exitError $ UnpackError err
    Pack (PackOptions directory) -> print directory

exitError :: NbpartsError -> IO a
exitError err = do
  TIO.hPutStrLn stderr $ renderError err
  exitWith $ ExitFailure 1

renderError :: NbpartsError -> T.Text
renderError err = case err of
  UnpackError (UnpackJSONDecodeError message) -> "Failed to parse notebook: " <> message
  UnpackError UnpackMissingCellIdError ->
    "Notebook contains cell(s) without an identifier. Try upgrading your notebook to at least version "
      <> T.show (fst recommendedNotebookFormat)
      <> "."
      <> T.show (snd recommendedNotebookFormat)
      <> "."
  UnpackError (UnpackPandocError pandocErr) -> Pandoc.renderError pandocErr
