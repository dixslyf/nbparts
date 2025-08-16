{-# LANGUAGE OverloadedStrings #-}

module Nbparts.Unpack where

import Control.Monad.Except (ExceptT (..), runExceptT)
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
import System.FilePath ((<.>), (</>))
import Text.Libyaml qualified as Libyaml

recommendedNotebookFormat :: (Int, Int)
recommendedNotebookFormat = (4, 5)

unpack :: FilePath -> IO (Either UnpackError ())
unpack notebookPath = runExceptT $ do
  notebookContents <- liftIO $ TIO.readFile notebookPath

  let exportDirectory = notebookPath <.> "nbparts"

  let sourceMediaSubdir = "media"
  liftIO $ Directory.createDirectoryIfMissing True (exportDirectory </> sourceMediaSubdir)

  liftIO $ Directory.createDirectoryIfMissing True exportDirectory

  let outputMediaSubdir = "outputs-media"
  liftIO $ Directory.createDirectoryIfMissing True (exportDirectory </> outputMediaSubdir)

  -- Collect and export metadata and outputs.
  let processNb :: Ipynb.Notebook a -> ExceptT UnpackError IO (Nbparts.Metadata, Nbparts.UnembeddedOutputs, [Nbparts.Source])
      processNb nb = do
        meta <- ExceptT $ pure $ Nbparts.collectMetadata nb
        outputs <- ExceptT $ pure $ Nbparts.collectOutputs nb
        unembeddedOutputs <- liftIO $ Nbparts.unembedOutputs exportDirectory outputMediaSubdir outputs
        sources <- ExceptT $ liftIO $ Nbparts.collectSources exportDirectory sourceMediaSubdir nb
        return (meta, unembeddedOutputs, sources)

  let notebookBytes = TE.encodeUtf8 notebookContents
  (metadata, outputs, sources) <-
    decodeNotebookThen
      processNb
      (ExceptT . pure . Left)
      notebookBytes

  let metadataPath = exportDirectory </> "metadata.yaml"
  liftIO $ Yaml.encodeFile metadataPath metadata

  let yamlOptions = Yaml.setStringStyle nbpartsYamlStringStyle Yaml.defaultEncodeOptions
  let outputsPath = exportDirectory </> "outputs.yaml"
  liftIO $ Yaml.encodeFileWith yamlOptions outputsPath outputs

  let sourcesPath = exportDirectory </> "sources.yaml"
  liftIO $ Yaml.encodeFileWith yamlOptions sourcesPath sources

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
nbpartsYamlStringStyle :: T.Text -> (Libyaml.Tag, Libyaml.Style)
nbpartsYamlStringStyle s
  | hasOnlyOneNewline s && hasNewlineSuffix s = (Libyaml.NoTag, Libyaml.DoubleQuoted)
  | "\n" `T.isInfixOf` s = (Libyaml.NoTag, Libyaml.Literal)
  | Yaml.isSpecialString s = (Libyaml.NoTag, Libyaml.SingleQuoted)
  | otherwise = (Libyaml.NoTag, Libyaml.PlainNoTag)
