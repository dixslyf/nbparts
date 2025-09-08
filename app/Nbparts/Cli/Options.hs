module Nbparts.Cli.Options where

import Control.Monad.Error.Class (throwError)
import Nbparts.Pack (PackOptions (PackOptions))
import Nbparts.Run (Command (Pack, Unpack), Options (Options))
import Nbparts.Types (Format (FormatJson, FormatMarkdown, FormatYaml))
import Nbparts.Unpack (UnpackOptions (UnpackOptions))
import Options.Applicative
  ( Parser,
    ReadM,
    argument,
    command,
    customExecParser,
    eitherReader,
    fullDesc,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    option,
    optional,
    prefs,
    progDesc,
    short,
    showHelpOnEmpty,
    str,
    strOption,
    value,
  )

unpackOptionsParser :: Parser UnpackOptions
unpackOptionsParser =
  UnpackOptions
    <$> argument str (metavar "NOTEBOOK" <> help "Path to the notebook to unpack")
    <*> option
      parseSourcesFormat
      ( short 'S'
          <> long "sources-format"
          <> metavar "yaml|json|markdown"
          <> help "Output format for sources"
          <> value FormatYaml
      )
    <*> option
      parseMetadataFormat
      ( short 'M'
          <> long "metadata-format"
          <> metavar "yaml|json"
          <> help "Output format for metadata"
          <> value FormatYaml
      )
    <*> option
      parseOutputsFormat
      ( short 'O'
          <> long "outputs-format"
          <> metavar "yaml|json"
          <> help "Output format for outputs"
          <> value FormatYaml
      )
    <*> optional
      ( strOption $
          short 'o'
            <> metavar "OUTPUT_PATH"
            <> help "Directory to write the unpacked notebook to"
      )

packOptionsParser :: Parser PackOptions
packOptionsParser =
  PackOptions
    <$> argument str (metavar "DIRECTORY" <> help "Path to the directory to pack into a notebook")
    <*> optional (strOption $ short 'o' <> metavar "OUTPUT_PATH" <> help "Path to write the notebook to")

commandParser :: Parser Command
commandParser =
  hsubparser $
    command "unpack" (info (Unpack <$> unpackOptionsParser) (progDesc "Unpack a notebook"))
      <> command "pack" (info (Pack <$> packOptionsParser) (progDesc "Pack a directory into a notebook"))

nbpartsOptionsParser :: Parser Options
nbpartsOptionsParser = Options <$> commandParser

parseSourcesFormat :: ReadM Format
parseSourcesFormat = eitherReader $ \case
  "yaml" -> pure FormatYaml
  "json" -> pure FormatJson
  "markdown" -> pure FormatMarkdown
  s -> throwError $ "Invalid sources format: " <> s

parseMetadataFormat :: ReadM Format
parseMetadataFormat = eitherReader $ \case
  "yaml" -> pure FormatYaml
  "json" -> pure FormatJson
  s -> throwError $ "Invalid sources format: " <> s

parseOutputsFormat :: ReadM Format
parseOutputsFormat = parseMetadataFormat

parseOpts :: IO Options
parseOpts =
  customExecParser (prefs showHelpOnEmpty) $
    info
      (helper <*> nbpartsOptionsParser)
      (fullDesc <> progDesc "Unpack a Jupyter notebook into its sources, metadata and outputs")
