module Tests.Integration.PackSpec where

import Control.Monad.Except (runExceptT)
import Data.Either qualified as Either
import Data.Text qualified as Text
import Nbparts.Pack
  ( PackOptions
      ( PackOptions,
        force,
        outputPath,
        partsDirectory
      ),
  )
import Nbparts.Types (Format
                        ( FormatJson,
                          FormatMarkdown,
                          FormatYaml
                        ), NbpartsError (UnpackError), renderError)
import Nbparts.Unpack
  ( UnpackOptions
      ( UnpackOptions,
        force,
        metadataFormat,
        notebookPath,
        outputPath,
        outputsFormat,
        sourcesFormat
      ),
  )
import Nbparts.Unpack.Outputs (collectOutputs)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, SpecWith, around, context, describe, it, shouldBe, shouldSatisfy)
import Tests.Integration.Util
  ( UnpackFormats
      ( UnpackFormats,
        metadataFormat,
        outputsFormat,
        sourcesFormat
      ),
    fixtureDir,
    readIpynb,
    runPack,
    runSpecWithUnpackFormatsCA,
    runUnpack,
  )

data PackTestOptions = PackTestOptions
  { unpackOutputPath :: Maybe FilePath,
    packOutputPath :: Maybe FilePath
  }

runTestUnpackWith' ::
  UnpackFormats ->
  Maybe FilePath ->
  FilePath ->
  IO (Either NbpartsError ())
runTestUnpackWith'
  (UnpackFormats {sourcesFormat, metadataFormat, outputsFormat})
  outputPath
  nbPath =
    runExceptT $
      runUnpack $
        UnpackOptions
          { notebookPath = nbPath,
            sourcesFormat,
            metadataFormat,
            outputsFormat,
            outputPath = outputPath,
            force = False
          }

runTestUnpackWith ::
  UnpackFormats ->
  FilePath ->
  Maybe FilePath ->
  FilePath ->
  IO (Either NbpartsError ())
runTestUnpackWith unpackFmts fixture relOutputPath tmpdir =
  runTestUnpackWith'
    unpackFmts
    (fmap (tmpdir </>) relOutputPath)
    (fixtureDir </> fixture)

runTestPack ::
  FilePath ->
  Maybe FilePath ->
  FilePath ->
  IO (Either NbpartsError ())
runTestPack
  relPartsDir
  relOutPath
  tmpdir =
    runExceptT $
      runPack $
        PackOptions
          { partsDirectory = tmpdir </> relPartsDir,
            outputPath = (</>) tmpdir <$> relOutPath,
            force = False
          }

runTests :: UnpackFormats -> SpecWith FilePath
runTests unpackFormats = do
  let runTestUnpack = runTestUnpackWith unpackFormats
      testDefaultOutputPath relPartsDir expectedPackOutputPath tmpdir = do
        -- Doesn't really matter what notebook we use.
        unpackRes <- runTestUnpack "empty.ipynb" (Just relPartsDir) tmpdir
        unpackRes `shouldSatisfy` Either.isRight

        packRes <- runTestPack relPartsDir Nothing tmpdir
        packRes `shouldSatisfy` Either.isRight

        exists <- Directory.doesFileExist $ tmpdir </> expectedPackOutputPath
        exists `shouldBe` True

  context "when given a parts directory path ending in `.ipynb.nbparts`" $
    it "should write a notebook to the path with `.nbparts` stripped" $ do
      testDefaultOutputPath "test.ipynb.nbparts" "test.ipynb"

  context "when given a parts directory path ending in `.nbparts` but not in `.ipynb.nbparts`" $
    it "should write a notebook to the path with `.nbparts` stripped and `.ipynb` appended" $ do
      testDefaultOutputPath "test.nbparts" "test.ipynb"

  context "when given a parts directory path ending in `.ipynb`" $
    it "should write a notebook to the path with `-packed` added before `.ipynb`" $ do
      testDefaultOutputPath "test.ipynb" "test-packed.ipynb"

  context "when given a parts directory path that does not end in `.nbparts` or `.ipynb`" $
    it "should write a notebook to the path with `.ipynb` appended" $ do
      _ <- testDefaultOutputPath "test" "test.ipynb"
      testDefaultOutputPath "test.hello" "test.hello.ipynb"

  context "when given a parts directory with no outputs file" $
    it "should pack as though the notebook has no outputs" $ \tmpdir -> do
      unpackRes <- runTestUnpack "stream-outputs.ipynb" (Just "unpacked") tmpdir
      unpackRes `shouldSatisfy` Either.isRight

      Directory.removeFile $
        tmpdir
          </> "unpacked"
          </> "outputs."
            <> case unpackFormats.outputsFormat of
              FormatYaml -> "yaml"
              FormatJson -> "json"
              FormatMarkdown -> error "Invalid format \"markdown\" for outputs"

      packRes <- runTestPack "unpacked" (Just "repacked.ipynb") tmpdir
      packRes `shouldSatisfy` Either.isRight

      eitherNb <- runExceptT $ readIpynb (tmpdir </> "repacked.ipynb")
      let nb = case eitherNb of
            Right n -> n
            Left err -> error err

      let collected = collectOutputs "outputs-media" nb
          outputs = case collected of
            Right (o, _media) -> o
            Left err -> error (Text.unpack $ renderError (UnpackError err))

      outputs `shouldBe` mempty

spec :: Spec
spec = around (withSystemTempDirectory "test-nbparts") $ do
  describe "Pack" $ do
    runSpecWithUnpackFormatsCA runTests
