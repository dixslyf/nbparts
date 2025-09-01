module Tests.Integration.UnpackPackSpec where

import Control.Monad.Except (runExceptT)
import Data.Either qualified as Either
import Nbparts.Pack (PackOptions (PackOptions))
import Nbparts.Pack qualified as Nbparts
import Nbparts.Unpack (UnpackOptions (UnpackOptions))
import Nbparts.Unpack qualified as Nbparts
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, around, context, describe, it, shouldBe, shouldSatisfy)
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

testIdentityWith' :: UnpackFormats -> FilePath -> FilePath -> Expectation
testIdentityWith' (UnpackFormats {sourcesFormat, metadataFormat, outputsFormat}) fixture tmpdir = do
  let nbPath = fixtureDir </> fixture

  let unpackPath = tmpdir </> "unpacked"
  let repackPath = tmpdir </> "repacked.ipynb"

  unpackResult <-
    runExceptT $
      runUnpack $
        UnpackOptions
          { notebook = nbPath,
            sourcesFormat,
            metadataFormat,
            outputsFormat,
            outputPath = Just unpackPath
          }
  unpackResult `shouldSatisfy` Either.isRight

  packResult <-
    runExceptT $
      runPack $
        PackOptions
          { directory = unpackPath,
            outputPath = Just repackPath
          }
  packResult `shouldSatisfy` Either.isRight

  nb <- runExceptT $ readIpynb nbPath
  repackedNb <- runExceptT $ readIpynb repackPath
  repackedNb `shouldBe` nb

testIdentityWith :: UnpackFormats -> FilePath -> SpecWith (Arg (FilePath -> Expectation))
testIdentityWith unpackFormats fixture = it "is identity" $ testIdentityWith' unpackFormats fixture

runTests :: UnpackFormats -> SpecWith FilePath
runTests unpackFormats = do
  let testIdentity = testIdentityWith unpackFormats

  context "when given an empty notebook (no sources, outputs or metadata)" $
    testIdentity "empty.ipynb"

  context "when given a notebook with erroneous outputs" $ do
    testIdentity "error-outputs.ipynb"

  context "when given a notebook with empty cells" $ do
    testIdentity "empty-cells.ipynb"

  context "when given a notebook containing the markdown spec" $
    testIdentity ("commonmark-spec" </> "commonmark-spec.ipynb")

  context "when given a notebook containing stream outputs" $
    testIdentity "stream-outputs.ipynb"

  context "when given a notebook containing image outputs" $
    testIdentity "image-outputs.ipynb"

  context "when given a notebook containing an interactive figure" $
    testIdentity "interactive-figure-outputs.ipynb"

  context "when given a notebook with raw cells" $ do
    testIdentity "raw-cells.ipynb"

  context "when given a notebook with latex" $ do
    testIdentity "latex.ipynb"

  context "when given a notebook containing markdown comments" $ do
    testIdentity "escape-markdown-comments.ipynb"

  context "when given a notebook with attachments" $
    testIdentity "attachments.ipynb"

  context "when given a notebook with duplicate attachments" $
    testIdentity "duplicate-attachments.ipynb"

  context "when given a notebook with a missing attachment" $
    testIdentity "missing-attachment.ipynb"

  context "when given a notebook containing attachments with unusual formatting" $
    testIdentity "attachments-unusual-formatting.ipynb"

  context "when given a notebook with an attachment that has multiple mime bundle entries" $
    testIdentity "attachments-multiple-mime-bundle-entries.ipynb"

spec :: Spec
spec = around (withSystemTempDirectory "test-nbparts") $ do
  describe "Unpack then pack" $ do
    runSpecWithUnpackFormatsCA runTests
