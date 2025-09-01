module Main where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExceptT)
import Data.Text.IO qualified as TIO
import Nbparts.Pack qualified as Nbparts
import Nbparts.Run qualified as Nbparts
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack qualified as Nbparts
import Options.Applicative qualified as OA
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

unpackOptionsParser :: OA.Parser Nbparts.UnpackOptions
unpackOptionsParser =
  Nbparts.UnpackOptions
    <$> OA.argument OA.str (OA.metavar "NOTEBOOK" <> OA.help "Path to the notebook to unpack")
    <*> OA.option
      parseSourcesFormat
      ( OA.short 'S'
          <> OA.long "sources-format"
          <> OA.metavar "yaml|json|markdown"
          <> OA.help "Output format for sources"
          <> OA.value Nbparts.FormatYaml
      )
    <*> OA.option
      parseMetadataFormat
      ( OA.short 'M'
          <> OA.long "metadata-format"
          <> OA.metavar "yaml|json"
          <> OA.help "Output format for metadata"
          <> OA.value Nbparts.FormatYaml
      )
    <*> OA.option
      parseOutputsFormat
      ( OA.short 'O'
          <> OA.long "outputs-format"
          <> OA.metavar "yaml|json"
          <> OA.help "Output format for outputs"
          <> OA.value Nbparts.FormatYaml
      )
    <*> OA.optional
      ( OA.strOption $
          OA.short 'o'
            <> OA.metavar "OUTPUT_PATH"
            <> OA.help "Directory to write the unpacked notebook to"
      )

packOptionsParser :: OA.Parser Nbparts.PackOptions
packOptionsParser =
  Nbparts.PackOptions
    <$> OA.argument OA.str (OA.metavar "DIRECTORY" <> OA.help "Path to the directory to pack into a notebook")
    <*> OA.optional (OA.strOption $ OA.short 'o' <> OA.metavar "OUTPUT_PATH" <> OA.help "Path to write the notebook to")

commandParser :: OA.Parser Nbparts.Command
commandParser =
  OA.hsubparser $
    OA.command "unpack" (OA.info (Nbparts.Unpack <$> unpackOptionsParser) (OA.progDesc "Unpack a notebook"))
      <> OA.command "pack" (OA.info (Nbparts.Pack <$> packOptionsParser) (OA.progDesc "Pack a directory into a notebook"))

nbpartsOptionsParser :: OA.Parser Nbparts.Options
nbpartsOptionsParser = Nbparts.Options <$> commandParser

parseSourcesFormat :: OA.ReadM Nbparts.Format
parseSourcesFormat = OA.eitherReader $ \case
  "yaml" -> pure Nbparts.FormatYaml
  "json" -> pure Nbparts.FormatJson
  "markdown" -> pure Nbparts.FormatMarkdown
  s -> throwError $ "Invalid sources format: " <> s

parseMetadataFormat :: OA.ReadM Nbparts.Format
parseMetadataFormat = OA.eitherReader $ \case
  "yaml" -> pure Nbparts.FormatYaml
  "json" -> pure Nbparts.FormatJson
  s -> throwError $ "Invalid sources format: " <> s

parseOutputsFormat :: OA.ReadM Nbparts.Format
parseOutputsFormat = parseMetadataFormat

parseOpts :: IO Nbparts.Options
parseOpts = OA.customExecParser prefs opts
  where
    prefs = OA.prefs OA.showHelpOnEmpty
    opts =
      OA.info
        (OA.helper <*> nbpartsOptionsParser)
        (OA.fullDesc <> OA.progDesc "Unpack a Jupyter notebook into its content, metadata and outputs")

main :: IO ()
main = do
  opts <- parseOpts
  result <- runExceptT $ Nbparts.run opts
  case result of
    Right _ -> pure ()
    Left err -> exitError err

exitError :: Nbparts.Error -> IO a
exitError err = do
  TIO.hPutStrLn stderr $ Nbparts.renderError err
  exitWith $ ExitFailure 1
