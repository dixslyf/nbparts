module Main where

import Control.Arrow (left)
import Control.Exception qualified as Exception
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExceptT)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Nbparts.Pack qualified as Nbparts
import Nbparts.Pack.Error qualified as Nbparts
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack qualified as Nbparts
import Nbparts.Unpack.Error qualified as Nbparts
import Options.Applicative qualified as OA
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)
import Text.Megaparsec qualified as Megaparsec

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

data NbpartsError = UnpackError Nbparts.UnpackError | PackError Nbparts.PackError

main :: IO ()
main = do
  (AppOptions command) <- parseOpts

  result <- case command of
    Unpack unpackOpts -> left UnpackError <$> runExceptT (Nbparts.unpack unpackOpts)
    Pack packOpts -> left PackError <$> runExceptT (Nbparts.pack packOpts)

  case result of
    Right _ -> pure ()
    Left err -> exitError err

exitError :: NbpartsError -> IO a
exitError err = do
  TIO.hPutStrLn stderr $ renderError err
  exitWith $ ExitFailure 1

renderError :: NbpartsError -> T.Text
renderError err = case err of
  UnpackError (Nbparts.UnpackJSONDecodeError message) -> "Failed to parse notebook: " <> message
  UnpackError (Nbparts.UnpackUnsupportedNotebookFormat (major, minor)) -> "Unsupported notebook format: " <> Text.show major <> "." <> Text.show minor
  UnpackError Nbparts.UnpackMissingCellIdError ->
    "Notebook contains cell(s) without an identifier. Try upgrading your notebook to at least version "
      <> T.show (fst Nbparts.recommendedNotebookFormat)
      <> "."
      <> T.show (snd Nbparts.recommendedNotebookFormat)
      <> "."
  UnpackError (Nbparts.UnpackMissingCellAttachmentError cellId attachmentName) ->
    "Could not find attachment \""
      <> attachmentName
      <> "\" for cell \""
      <> cellId
      <> "\""
  PackError (Nbparts.PackUnsupportedNotebookFormat (major, minor)) -> "Unsupported notebook format: " <> Text.show major <> "." <> Text.show minor
  PackError (Nbparts.PackParseManifestError parseErr) -> "Failed to parse manifest: " <> T.pack (Exception.displayException parseErr)
  PackError (Nbparts.PackParseYamlSourcesError parseErr) -> "Failed to parse sources: " <> T.pack (Exception.displayException parseErr)
  PackError (Nbparts.PackParseMarkdownSourcesError errBundle) -> Text.pack $ Megaparsec.errorBundlePretty errBundle
  PackError (Nbparts.PackParseMetadataError parseErr) -> "Failed to parse metadata: " <> T.pack (Exception.displayException parseErr)
  PackError (Nbparts.PackParseOutputsError parseErr) -> "Failed to parse outputs: " <> T.pack (Exception.displayException parseErr)
  PackError Nbparts.PackMissingCellIdError -> "Markdown content contains missing cell ID"
  PackError (Nbparts.PackMissingCellMetadataError cellId) -> "Could not find metadata for cell ID: " <> cellId
  PackError (Nbparts.PackMissingCellOutputsError cellId) -> "Could not find outputs for cell ID: " <> cellId
  PackError (Nbparts.PackCellMetadataTypeMismatch expected actual) ->
    "Cell metadata type mismatch. Expected: "
      <> renderCellMetadataTag expected
      <> ", but got: "
      <> renderCellMetadataTag actual

renderCellMetadataTag :: Nbparts.CellMetadataTag -> T.Text
renderCellMetadataTag Nbparts.CodeCellMetadataTag = "code cell metadata"
renderCellMetadataTag Nbparts.GenericCellMetadataTag = "generic cell metadata"
