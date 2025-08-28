module Tests.Integration.UnpackSpec where

import Control.Monad.Except (runExceptT)
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack (UnpackOptions (UnpackOptions))
import Nbparts.Unpack qualified as Nbparts
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Expectation, Spec, SpecWith, around, context, describe, it, shouldBe, shouldSatisfy)
import Tests.Integration.Util (fixtureDir, runUnpack)

testUnpackWith :: Nbparts.Format -> FilePath -> (Either Nbparts.Error () -> Expectation) -> FilePath -> Expectation
testUnpackWith sourcesFormat fixture predicate tmpdir = do
  let nbPath = fixtureDir </> fixture
  let unpackPath = tmpdir </> "unpacked"
  unpackResult <-
    runExceptT $
      runUnpack $
        UnpackOptions
          { notebook = nbPath,
            sourcesFormat,
            outputPath = Just unpackPath
          }
  predicate unpackResult

runTests :: Nbparts.Format -> SpecWith FilePath
runTests fmt = do
  let testUnpack = testUnpackWith fmt

  context "when given a notebook with missing cell IDs" $
    it "should return a missing cell ID error" $
      testUnpack "missing-cell-ids.ipynb" $
        shouldBe $
          Left (Nbparts.UnpackError Nbparts.UnpackMissingCellIdError)

  context "when given a malformed notebook" $
    it "should return a parse error" $
      testUnpack "malformed.ipynb" $ \res ->
        res `shouldSatisfy` \case
          Left (Nbparts.UnpackError (Nbparts.UnpackParseNotebookError _)) -> True
          _ -> False

  context "when given an empty file" $
    it "should return a parse error" $
      testUnpack "null.ipynb" $ \res ->
        res `shouldSatisfy` \case
          Left (Nbparts.UnpackError (Nbparts.UnpackParseNotebookError _)) -> True
          _ -> False

  context "when given a v3 notebook" $
    it "should return an unsupported notebook error" $
      testUnpack "v3.ipynb" $ \res ->
        res `shouldSatisfy` \case
          Left (Nbparts.UnpackError (Nbparts.UnpackUnsupportedNotebookFormat (3, 0))) -> True
          _ -> False

spec :: Spec
spec = around (withSystemTempDirectory "test-nbparts") $ do
  describe "Unpack then pack" $ do
    context "when exporting sources to YAML" $ runTests Nbparts.FormatYaml
    context "when exporting sources to Markdown" $ runTests Nbparts.FormatMarkdown
