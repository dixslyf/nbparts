module Tests.Integration.UnpackSpec where

import Control.Monad.Except (runExceptT)
import Nbparts.Types
  ( NbpartsError (UnpackError),
    UnpackError
      ( UnpackMissingCellIdError,
        UnpackParseNotebookError,
        UnpackUnsupportedNotebookFormat
      ),
  )
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
    runSpecWithUnpackFormatsCA,
    runUnpack,
  )

runTestUnpackWith :: UnpackFormats -> FilePath -> FilePath -> IO (Either NbpartsError ())
runTestUnpackWith (UnpackFormats {sourcesFormat, metadataFormat, outputsFormat}) fixture tmpdir = do
  let nbPath = fixtureDir </> fixture
  let unpackPath = tmpdir </> "unpacked"
  runExceptT $
    runUnpack $
      UnpackOptions
        { notebookPath = nbPath,
          sourcesFormat,
          metadataFormat,
          outputsFormat,
          outputPath = Just unpackPath,
          force = False
        }

runTests :: UnpackFormats -> SpecWith FilePath
runTests fmts = do
  let runTestUnpack = runTestUnpackWith fmts

  context "when given a notebook with missing cell IDs" $
    it "should return a missing cell ID error" $ \tmpdir -> do
      res <- runTestUnpack "missing-cell-ids.ipynb" tmpdir
      res `shouldBe` Left (UnpackError UnpackMissingCellIdError)

  context "when given a malformed notebook" $
    it "should return a parse error" $ \tmpdir -> do
      res <- runTestUnpack "malformed.ipynb" tmpdir
      res `shouldSatisfy` \case
        Left (UnpackError (UnpackParseNotebookError _)) -> True
        _ -> False

  context "when given an empty file" $
    it "should return a parse error" $ \tmpdir -> do
      res <- runTestUnpack "null.ipynb" tmpdir
      res `shouldSatisfy` \case
        Left (UnpackError (UnpackParseNotebookError _)) -> True
        _ -> False

  context "when given a v3 notebook" $
    it "should return an unsupported notebook error" $ \tmpdir -> do
      res <- runTestUnpack "v3.ipynb" tmpdir
      res `shouldBe` Left (UnpackError (UnpackUnsupportedNotebookFormat (3, 0)))

spec :: Spec
spec = around (withSystemTempDirectory "test-nbparts") $ do
  describe "Unpack" $ do
    runSpecWithUnpackFormatsCA runTests
