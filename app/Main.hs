module Main where

import Control.Arrow (left)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExceptT)
import Data.Text.IO qualified as TIO
import Nbparts.Pack qualified as Nbparts
import Nbparts.Types (NbpartsError)
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack qualified as Nbparts
import Options.Applicative qualified as OA
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

newtype AppOptions = AppOptions
  { command :: Command
  }

data Command = Unpack Nbparts.UnpackOptions | Pack Nbparts.PackOptions

unpackOptionsParser :: OA.Parser Nbparts.UnpackOptions
unpackOptionsParser =
  Nbparts.UnpackOptions
    <$> OA.argument OA.str (OA.metavar "NOTEBOOK" <> OA.help "Path to the notebook to unpack")
    <*> OA.option
      parseSourcesFormat
      ( OA.short 's'
          <> OA.long "source-format"
          <> OA.metavar "yaml|markdown"
          <> OA.help "Output format for sources"
          <> OA.value Nbparts.FormatYaml
      )

packOptionsParser :: OA.Parser Nbparts.PackOptions
packOptionsParser =
  Nbparts.PackOptions
    <$> OA.argument OA.str (OA.metavar "DIRECTORY" <> OA.help "Path to the directory to pack into a notebook")
    <*> OA.optional (OA.strOption $ OA.short 'o' <> OA.metavar "OUTPUT_PATH" <> OA.help "Path to write the notebook to")

commandParser :: OA.Parser Command
commandParser =
  OA.hsubparser $
    OA.command "unpack" (OA.info (Unpack <$> unpackOptionsParser) (OA.progDesc "Unpack a notebook"))
      <> OA.command "pack" (OA.info (Pack <$> packOptionsParser) (OA.progDesc "Pack a directory into a notebook"))

appOptionsParser :: OA.Parser AppOptions
appOptionsParser = AppOptions <$> commandParser

parseSourcesFormat :: OA.ReadM Nbparts.Format
parseSourcesFormat = OA.eitherReader $ \case
  "yaml" -> pure Nbparts.FormatYaml
  "markdown" -> pure Nbparts.FormatMarkdown
  s -> throwError $ "Invalid sources format: " <> s

parseOpts :: IO AppOptions
parseOpts = OA.customExecParser prefs opts
  where
    prefs = OA.prefs OA.showHelpOnEmpty
    opts = OA.info (OA.helper <*> appOptionsParser) (OA.fullDesc <> OA.progDesc "Unpack a Jupyter notebook into its content, metadata and outputs")

main :: IO ()
main = do
  (AppOptions command) <- parseOpts

  result <- case command of
    Unpack unpackOpts -> left Nbparts.UnpackError <$> runExceptT (Nbparts.unpack unpackOpts)
    Pack packOpts -> left Nbparts.PackError <$> runExceptT (Nbparts.pack packOpts)

  case result of
    Right _ -> pure ()
    Left err -> exitError err

exitError :: NbpartsError -> IO a
exitError err = do
  TIO.hPutStrLn stderr $ Nbparts.renderError err
  exitWith $ ExitFailure 1
