{-# LANGUAGE OverloadedStrings #-}

module Nbparts.Unpack where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Ipynb qualified as Ipynb
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Nbparts.Types (NbpartsFormat)
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

data UnpackOptions = UnpackOptions
  { notebook :: FilePath,
    sourcesFormat :: NbpartsFormat
  }

unpack :: (MonadError UnpackError m, MonadIO m) => UnpackOptions -> m ()
unpack (UnpackOptions notebookPath sourcesFormat) = do
  notebookContents <- liftIO $ TIO.readFile notebookPath

  let exportDirectory = notebookPath <.> "nbparts"
  let sourceMediaSubdir = "media"
  let outputMediaSubdir = "outputs-media"
  liftIO $ do
    Directory.createDirectoryIfMissing True (exportDirectory </> sourceMediaSubdir)
    Directory.createDirectoryIfMissing True exportDirectory
    Directory.createDirectoryIfMissing True (exportDirectory </> outputMediaSubdir)

  -- Parse the notebook.
  let notebookBytes = TE.encodeUtf8 notebookContents
  (nb :: Nbparts.SomeNotebook) <-
    liftEither $
      left (Nbparts.UnpackJSONDecodeError . T.pack) $
        Aeson.eitherDecodeStrict notebookBytes

  -- Collect sources, metadata and outputs.
  let manifest = Nbparts.mkNbpartsManifest sourcesFormat
  let withNb = Nbparts.withSomeNotebook nb
  metadata <- liftEither $ withNb Nbparts.collectMetadata
  sources <- withNb (Nbparts.collectSources exportDirectory sourceMediaSubdir)
  outputs <- withNb (liftEither . Nbparts.collectOutputs >=> liftIO . Nbparts.unembedOutputs exportDirectory outputMediaSubdir)

  -- Export manifest, sources, metadata and outputs.
  let yamlOptions = Yaml.setStringStyle nbpartsYamlStringStyle Yaml.defaultEncodeOptions
  let manifestPath = exportDirectory </> "nbparts.yaml"
  let metadataPath = exportDirectory </> "metadata.yaml"
  let outputsPath = exportDirectory </> "outputs.yaml"
  let sourcesPath =
        exportDirectory
          </> ( "sources" <> case sourcesFormat of
                  Nbparts.FormatYaml -> ".yaml"
                  Nbparts.FormatMarkdown -> ".md"
              )

  liftIO $ do
    Yaml.encodeFile manifestPath manifest
    Yaml.encodeFile metadataPath metadata
    Yaml.encodeFileWith yamlOptions sourcesPath sources

  case sourcesFormat of
    Nbparts.FormatYaml -> liftIO $ Yaml.encodeFileWith yamlOptions outputsPath outputs
    Nbparts.FormatMarkdown -> do
      let lang = Maybe.fromMaybe "" $ extractLanguage metadata
      markdownText <- liftEither $ Nbparts.sourcesToMarkdown lang sources
      liftIO $ Text.writeFile sourcesPath markdownText

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
