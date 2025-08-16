{-# LANGUAGE OverloadedStrings #-}

module Nbparts.Unpack where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Except qualified as ExceptT
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Ipynb qualified as Ipynb
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Yaml
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error (UnpackError)
import Nbparts.Unpack.Error qualified as Nbparts
import Nbparts.Unpack.Metadata qualified as Nbparts
import Nbparts.Unpack.Outputs qualified as Nbparts
import Nbparts.Unpack.Sources qualified as Nbparts
import System.Directory qualified as Directory
import System.FilePath ((-<.>), (<.>), (</>))
import System.FilePath qualified as FilePath
import Text.Libyaml qualified as Libyaml
import Text.Pandoc (WriterOptions (..), def, pandocExtensions)
import Text.Pandoc qualified as Pandoc

recommendedNotebookFormat :: (Int, Int)
recommendedNotebookFormat = (4, 5)

-- NOTE: Currently, we parse the notebook twice: once by Pandoc's `readIpynb`
-- and once more using `Aeson.eitherDecodeStrict`. I don't think there is a way
-- to avoid this since Pandoc does not expose an API to create a document directly
-- from an `Ipynb.Notebook`. We also cannot use the Pandoc document's metadata since
-- we lose type information about numbers and null (i.e., lossy).
unpack :: FilePath -> IO (Either UnpackError ())
unpack notebookPath = runExceptT $ do
  notebookContents <- liftIO $ TIO.readFile notebookPath

  let exportDirectory = notebookPath <.> "nbparts"
  liftIO $ Directory.createDirectoryIfMissing True exportDirectory

  let outputMediaSubdir = "outputs-media"
  liftIO $ Directory.createDirectoryIfMissing True (exportDirectory </> outputMediaSubdir)

  let extractMetadataAndOutputs :: Ipynb.Notebook a -> ExceptT UnpackError IO (Nbparts.Metadata, Nbparts.UnembeddedOutputs)
      extractMetadataAndOutputs nb = do
        meta <- ExceptT $ pure $ Nbparts.collectMetadata nb
        outputs <- ExceptT $ pure $ Nbparts.collectOutputs nb
        unembeddedOutputs <- liftIO $ Nbparts.unembedOutputs exportDirectory outputMediaSubdir outputs
        return (meta, unembeddedOutputs)

  let notebookBytes = TE.encodeUtf8 notebookContents
  (metadata, outputs) <-
    decodeNotebookThen
      extractMetadataAndOutputs
      (ExceptT . pure . Left)
      notebookBytes

  -- Export metadata and outputs.
  let metadataPath = exportDirectory </> "metadata.yaml"
  liftIO $ Yaml.encodeFile metadataPath metadata
  let outputsPath = exportDirectory </> "outputs.yaml"
  let outputsYamlOptions = Yaml.setStringStyle outputsYamlStringStyle Yaml.defaultEncodeOptions
  liftIO $ Yaml.encodeFileWith outputsYamlOptions outputsPath outputs

  -- Convert to markdown, extract outputs and export authored attachments.
  convertResult <-
    liftIO $
      Pandoc.runIO $
        do
          doc <- Pandoc.readIpynb def notebookContents
          mediaAdjustedDoc <- Nbparts.extractAuthoredMedia exportDirectory "media" doc
          let processedDoc = Nbparts.removeCellMetadata . Nbparts.removeCellOutputs $ mediaAdjustedDoc
          Pandoc.writeMarkdown def {writerExtensions = pandocExtensions} processedDoc

  markdownText <- case convertResult of
    Right markdown -> pure markdown
    Left err -> ExceptT.throwError $ Nbparts.UnpackPandocError err

  -- Export markdown.
  let markdownPath = exportDirectory </> (FilePath.takeFileName notebookPath -<.> ".md")
  liftIO $ TIO.writeFile markdownPath markdownText

decodeNotebookThen :: (forall a. (Aeson.ToJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> b) -> (UnpackError -> b) -> ByteString -> b
decodeNotebookThen onSuccess onError bytes =
  case Aeson.eitherDecodeStrict bytes of
    Right (nb :: Ipynb.Notebook Ipynb.NbV4) -> onSuccess nb
    Left _ ->
      case Aeson.eitherDecodeStrict bytes of
        Right (nb :: Ipynb.Notebook Ipynb.NbV3) -> onSuccess nb
        Left message -> onError (Nbparts.UnpackJSONDecodeError (T.pack message))

hasOnlyOneNewline :: T.Text -> Bool
hasOnlyOneNewline text = T.length (T.filter (== '\n') text) == 1

hasNewlineSuffix :: T.Text -> Bool
hasNewlineSuffix = T.isSuffixOf "\n"

-- Based on Yaml's default string style.
outputsYamlStringStyle :: T.Text -> (Libyaml.Tag, Libyaml.Style)
outputsYamlStringStyle s
  | hasOnlyOneNewline s && hasNewlineSuffix s = (Libyaml.NoTag, Libyaml.DoubleQuoted)
  | "\n" `T.isInfixOf` s = (Libyaml.NoTag, Libyaml.Literal)
  | Yaml.isSpecialString s = (Libyaml.NoTag, Libyaml.SingleQuoted)
  | otherwise = (Libyaml.NoTag, Libyaml.PlainNoTag)
