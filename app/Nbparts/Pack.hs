module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
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

  let processNb :: Ipynb.Notebook a -> (FilePath -> Ipynb.Notebook a -> IO ()) -> ExceptT PackError IO ()
      processNb nb encode = do
        nbWithMeta <- ExceptT $ pure $ Nbparts.fillMetadata nb metadata
        nbWithMetaAndOutputs <- ExceptT $ Nbparts.fillOutputs nbpartsDir unembeddedOutputs nbWithMeta
        liftIO $ encode outputPath nbWithMetaAndOutputs

  let ipynbBytes = Text.encodeUtf8 ipynbText
  case Aeson.eitherDecodeStrict ipynbBytes of
    Right (nb :: Ipynb.Notebook Ipynb.NbV4) -> processNb nb Aeson.encodeFile
    Left _ ->
      case Aeson.eitherDecodeStrict ipynbBytes of
        Right (nb :: Ipynb.Notebook Ipynb.NbV3) -> processNb nb Aeson.encodeFile
        Left message -> ExceptT $ pure $ Left $ Nbparts.PackParseIpynbError (Text.pack message)
