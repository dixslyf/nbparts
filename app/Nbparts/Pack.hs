module Nbparts.Pack where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe qualified as Maybe
import Data.Text.IO qualified as Text
import Nbparts.Pack.Error (PackError)
import Nbparts.Pack.Error qualified as Nbparts
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

  let mdPath = nbpartsDir </> basename <.> "md"
  mdText <- liftIO $ Text.readFile mdPath
  ipynbText <- ExceptT $ fmap (left Nbparts.PackPandocError) $ Pandoc.runIO $ do
    doc <- Pandoc.readMarkdown def {readerExtensions = Pandoc.pandocExtensions} mdText
    Pandoc.writeIpynb def doc

  liftIO $ Text.writeFile outputPath ipynbText
