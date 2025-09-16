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
import Test.Hspec (Expectation, Spec, SpecWith, around, context, describe, it, shouldBe, shouldSatisfy)
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

testUnpackWith :: UnpackFormats -> FilePath -> (Either NbpartsError () -> Expectation) -> FilePath -> Expectation
testUnpackWith (UnpackFormats {sourcesFormat, metadataFormat, outputsFormat}) fixture predicate tmpdir = do
  let nbPath = fixtureDir </> fixture
  let unpackPath = tmpdir </> "unpacked"
  unpackResult <-
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
  predicate unpackResult

runTests :: UnpackFormats -> SpecWith FilePath
runTests fmts = do
  let testUnpack = testUnpackWith fmts

  context "when given a notebook with missing cell IDs" $
    it "should return a missing cell ID error" $
      testUnpack "missing-cell-ids.ipynb" $
        shouldBe $
          Left (UnpackError UnpackMissingCellIdError)

  context "when given a malformed notebook" $
    it "should return a parse error" $
      testUnpack "malformed.ipynb" $ \res ->
        res `shouldSatisfy` \case
          Left (UnpackError (UnpackParseNotebookError _)) -> True
          _ -> False

  context "when given an empty file" $
    it "should return a parse error" $
      testUnpack "null.ipynb" $ \res ->
        res `shouldSatisfy` \case
          Left (UnpackError (UnpackParseNotebookError _)) -> True
          _ -> False

  context "when given a v3 notebook" $
    it "should return an unsupported notebook error" $
      testUnpack "v3.ipynb" $ \res ->
        res `shouldSatisfy` \case
          Left (UnpackError (UnpackUnsupportedNotebookFormat (3, 0))) -> True
          _ -> False

spec :: Spec
spec = around (withSystemTempDirectory "test-nbparts") $ do
  describe "Unpack" $ do
    runSpecWithUnpackFormatsCA runTests
