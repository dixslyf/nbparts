module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (..))
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Ipynb qualified as Ipynb
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Nbparts.Pack.Error (PackError)
import Nbparts.Pack.Error qualified as Nbparts
import Nbparts.Pack.Metadata qualified as Nbparts
import Nbparts.Pack.Outputs qualified as Nbparts
import Nbparts.Types qualified as Nbparts
import System.FilePath ((<.>), (</>))
import System.FilePath qualified as FilePath
import Text.Pandoc (ReaderOptions (..))
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Options (def)

pack :: FilePath -> Maybe FilePath -> IO (Either PackError ())
pack nbpartsDir maybeOutputPath = runExceptT $ do
  -- `nbpartsDir` should be in the form "some_notebook.ipynb.nbparts".
  let fallbackOutputPath = FilePath.dropExtension nbpartsDir
  let basename = FilePath.dropExtension $ FilePath.takeBaseName fallbackOutputPath
  let outputPath = Maybe.fromMaybe fallbackOutputPath maybeOutputPath

  -- Convert the markdown into ipynb.
  let mdPath = nbpartsDir </> basename <.> "md"
  mdText <- liftIO $ Text.readFile mdPath
  ipynbText <- ExceptT $ fmap (left Nbparts.PackPandocError) $ Pandoc.runIO $ do
    Pandoc.setResourcePath [nbpartsDir]
    doc <- Pandoc.readMarkdown def {readerExtensions = Pandoc.pandocExtensions} mdText
    Pandoc.writeIpynb def doc

  -- Read metadata.
  -- TODO: Don't fail if file is missing — just warn.
  let metadataPath = nbpartsDir </> "metadata.yaml"
  (metadata :: Nbparts.Metadata) <- ExceptT (left Nbparts.PackParseMetadataError <$> Yaml.decodeFileEither metadataPath)

  -- Read outputs.
  let outputsPath = nbpartsDir </> "outputs.yaml"
  (unembeddedOutputs :: Nbparts.UnembeddedOutputs) <- ExceptT (left Nbparts.PackParseOutputsError <$> Yaml.decodeFileEither outputsPath)

  let processNb :: (Aeson.ToJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> ExceptT PackError IO ()
      processNb nb = do
        nbWithMeta <- ExceptT $ pure $ Nbparts.fillMetadata nb metadata
        nbWithMetaAndOutputs <- ExceptT $ Nbparts.fillOutputs nbpartsDir unembeddedOutputs nbWithMeta
        liftIO $ exportNotebook outputPath nbWithMetaAndOutputs

  let ipynbBytes = Text.encodeUtf8 ipynbText
  decodeNotebookThen
    processNb
    (ExceptT . pure . Left)
    ipynbBytes

decodeNotebookThen :: (forall a. (Aeson.ToJSON (Ipynb.Notebook a)) => Ipynb.Notebook a -> b) -> (PackError -> b) -> ByteString -> b
decodeNotebookThen onSuccess onError bytes =
  case Aeson.eitherDecodeStrict bytes of
    Right (nb :: Ipynb.Notebook Ipynb.NbV4) -> onSuccess nb
    Left _ ->
      case Aeson.eitherDecodeStrict bytes of
        Right (nb :: Ipynb.Notebook Ipynb.NbV3) -> onSuccess nb
        Left message -> onError (Nbparts.PackParseIpynbError (Text.pack message))

prettyConfig :: AesonPretty.Config
prettyConfig = AesonPretty.defConfig {confIndent = AesonPretty.Spaces 1}

exportNotebook :: (Aeson.ToJSON (Ipynb.Notebook a)) => FilePath -> Ipynb.Notebook a -> IO ()
exportNotebook fp = LazyByteString.writeFile fp . AesonPretty.encodePretty' prettyConfig
