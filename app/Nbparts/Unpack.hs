{-# LANGUAGE OverloadedStrings #-}

module Nbparts.Unpack where

import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack.Error (UnpackError)
import Nbparts.Unpack.Error qualified as Nbparts
import Nbparts.Unpack.Metadata qualified as Nbparts
import Nbparts.Unpack.Outputs qualified as Nbparts
import Nbparts.Unpack.Sources qualified as Nbparts
import Nbparts.Unpack.Sources.Markdown qualified as Nbparts
import System.Directory qualified as Directory
import System.FilePath ((<.>), (</>))
import Text.Libyaml qualified as Libyaml

recommendedNotebookFormat :: (Int, Int)
recommendedNotebookFormat = (4, 5)

unpack :: (MonadError UnpackError m, MonadIO m) => FilePath -> m ()
unpack notebookPath = do
  notebookContents <- liftIO $ TIO.readFile notebookPath

  let exportDirectory = notebookPath <.> "nbparts"
  let sourceMediaSubdir = "media"
  let outputMediaSubdir = "outputs-media"
  liftIO $ do
    Directory.createDirectoryIfMissing True (exportDirectory </> sourceMediaSubdir)
    Directory.createDirectoryIfMissing True exportDirectory
    Directory.createDirectoryIfMissing True (exportDirectory </> outputMediaSubdir)

  -- Collect and export metadata and outputs.
  let processNb :: (MonadError UnpackError m, MonadIO m) => Ipynb.Notebook a -> m (Nbparts.Metadata, [Nbparts.Source], Nbparts.UnembeddedOutputs)
      processNb nb = do
        meta <- liftEither $ Nbparts.collectMetadata nb
        sources <- Nbparts.collectSources exportDirectory sourceMediaSubdir nb
        outputs <- liftEither $ Nbparts.collectOutputs nb
        unembeddedOutputs <- liftIO $ Nbparts.unembedOutputs exportDirectory outputMediaSubdir outputs
        return (meta, sources, unembeddedOutputs)

  let notebookBytes = TE.encodeUtf8 notebookContents
  (metadata, sources, outputs) <- decodeNotebookThen processNb notebookBytes

  let yamlOptions = Yaml.setStringStyle nbpartsYamlStringStyle Yaml.defaultEncodeOptions
  let metadataPath = exportDirectory </> "metadata.yaml"
  let sourcesPath = exportDirectory </> "sources.yaml"
  let sourcesMdPath = exportDirectory </> "sources.md"
  let outputsPath = exportDirectory </> "outputs.yaml"

  let lang = Maybe.fromMaybe "" $ extractLanguage metadata
  let markdownText = Nbparts.sourcesToMarkdown lang sources

  liftIO $ do
    Yaml.encodeFile metadataPath metadata
    Yaml.encodeFileWith yamlOptions sourcesPath sources
    Yaml.encodeFileWith yamlOptions outputsPath outputs
    Text.writeFile sourcesMdPath markdownText

decodeNotebookThen ::
  (MonadError UnpackError m) =>
  (forall a. (Aeson.ToJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> m b) ->
  ByteString ->
  m b
decodeNotebookThen onSuccess bytes =
  case Aeson.eitherDecodeStrict bytes of
    Right (nb :: Ipynb.Notebook Ipynb.NbV4) -> onSuccess nb
    Left _ ->
      case Aeson.eitherDecodeStrict bytes of
        Right (nb :: Ipynb.Notebook Ipynb.NbV3) -> onSuccess nb
        Left message -> throwError $ Nbparts.UnpackJSONDecodeError (T.pack message)

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

extractLanguage :: Nbparts.Metadata -> Maybe T.Text
extractLanguage (Nbparts.Metadata _ _ (Ipynb.JSONMeta nbMeta) _) = do
  kernelspec <- Map.lookup "kernelspec" nbMeta
  langFromKernelSpec kernelspec

langFromKernelSpec :: Aeson.Value -> Maybe T.Text
langFromKernelSpec (Aeson.Object obj) = case Aeson.KeyMap.lookup "language" obj of
  Just (Aeson.String lang) -> Just lang
  _ -> Nothing
langFromKernelSpec _ = Nothing
