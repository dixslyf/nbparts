{-# LANGUAGE DataKinds #-}

module Tests.Integration.UnpackPackSpec where

import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either qualified as Either
import Data.Text (Text)
import Nbparts.Pack
  ( PackOptions
      ( PackOptions,
        outputPath,
        partsDirectory
      ),
  )
import Nbparts.Unpack
  ( UnpackOptions
      ( UnpackOptions,
        metadataFormat,
        notebookPath,
        outputPath,
        outputsFormat,
        sourcesFormat
      ),
  )
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    Scheme (Https),
    Url,
    defaultHttpConfig,
    https,
    lbsResponse,
    req,
    responseBody,
    runReq,
    (/:),
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Arg,
    Expectation,
    Spec,
    SpecWith,
    around,
    context,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )
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
testIdentityWith' (UnpackFormats {sourcesFormat, metadataFormat, outputsFormat}) nbPath tmpdir = do
  let unpackPath = tmpdir </> "unpacked"
  let repackPath = tmpdir </> "repacked.ipynb"

  unpackResult <-
    runExceptT $
      runUnpack $
        UnpackOptions
          { notebookPath = nbPath,
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
          { partsDirectory = unpackPath,
            outputPath = Just repackPath
          }
  packResult `shouldSatisfy` Either.isRight

  nb <- runExceptT $ readIpynb nbPath
  repackedNb <- runExceptT $ readIpynb repackPath
  repackedNb `shouldBe` nb

testNetIdentityWith' :: UnpackFormats -> Url scheme -> FilePath -> Expectation
testNetIdentityWith' unpackFormats url tmpdir = do
  lbs <- runReq defaultHttpConfig $ req GET url NoReqBody lbsResponse mempty
  let nbBytes = responseBody lbs

  let nbPath = tmpdir </> "nb.ipynb"
  LazyByteString.writeFile nbPath nbBytes

  testIdentityWith' unpackFormats nbPath tmpdir

testNetIdentityWith :: UnpackFormats -> Url scheme -> SpecWith (Arg (FilePath -> Expectation))
testNetIdentityWith unpackFormats url = it "is identity" $ testNetIdentityWith' unpackFormats url

testIdentityWith :: UnpackFormats -> FilePath -> SpecWith (Arg (FilePath -> Expectation))
testIdentityWith unpackFormats nbpath = it "is identity" $ testIdentityWith' unpackFormats nbpath

testFixtureIdentityWith :: UnpackFormats -> FilePath -> SpecWith (Arg (FilePath -> Expectation))
testFixtureIdentityWith unpackFormats fixture = testIdentityWith unpackFormats $ fixtureDir </> fixture

data GitHubReleaseFile = GitHubReleaseFile
  { owner :: Text,
    repo :: Text,
    tag :: Text,
    file :: Text
  }

mkGitHubReleaseFileUrl :: GitHubReleaseFile -> Url 'Https
mkGitHubReleaseFileUrl GitHubReleaseFile {owner, repo, tag, file} =
  https "github.com"
    /: owner
    /: repo
    /: "releases"
    /: "download"
    /: tag
    /: file

runTests :: UnpackFormats -> SpecWith FilePath
runTests unpackFormats = do
  let testFixtureIdentity = testFixtureIdentityWith unpackFormats
  let testNetIdentity = testNetIdentityWith unpackFormats

  context "when given an empty notebook (no sources, outputs or metadata)" $
    testFixtureIdentity "empty.ipynb"

  context "when given a notebook with erroneous outputs" $ do
    testFixtureIdentity "error-outputs.ipynb"

  context "when given a notebook with empty cells" $ do
    testFixtureIdentity "empty-cells.ipynb"

  context "when given a notebook containing the markdown spec" $
    testFixtureIdentity ("commonmark-spec" </> "commonmark-spec.ipynb")

  context "when given a notebook containing stream outputs" $
    testFixtureIdentity "stream-outputs.ipynb"

  context "when given a notebook containing image outputs" $
    testFixtureIdentity "image-outputs.ipynb"

  context "when given a notebook containing an interactive figure" $
    testFixtureIdentity "interactive-figure-outputs.ipynb"

  context "when given a notebook with raw cells" $ do
    testFixtureIdentity "raw-cells.ipynb"

  context "when given a notebook with latex" $ do
    testFixtureIdentity "latex.ipynb"

  context "when given a notebook containing markdown comments" $ do
    testFixtureIdentity "escape-markdown-comments.ipynb"

  context "when given a notebook with attachments" $
    testFixtureIdentity "attachments.ipynb"

  context "when given a notebook with duplicate attachments" $
    testFixtureIdentity "duplicate-attachments.ipynb"

  context "when given a notebook with a missing attachment" $
    testFixtureIdentity "missing-attachment.ipynb"

  context "when given a notebook containing attachments with unusual formatting" $
    testFixtureIdentity "attachments-unusual-formatting.ipynb"

  context "when given a notebook with an attachment that has multiple mime bundle entries" $
    testFixtureIdentity "attachments-multiple-mime-bundle-entries.ipynb"

  context "when given a notebook containing reference-link attachments" $
    testFixtureIdentity "attachments-reference.ipynb"

  context "when given a notebook containing reference-link attachments with unusual formatting" $
    testFixtureIdentity "attachments-reference-unusual-formatting.ipynb"

  context "when given a real-world notebook (mlp-vgg16-fashion.ipynb)" $
    testNetIdentity $
      mkGitHubReleaseFileUrl
        GitHubReleaseFile
          { owner = "dixslyf",
            repo = "mlp-vgg16-fashion",
            tag = "v1.0.0",
            file = "mlp-vgg16-fashion.ipynb"
          }

  context "when given a real-world notebook (unet-nuclei-instance-segmentation.ipynb)" $
    testNetIdentity $
      mkGitHubReleaseFileUrl
        GitHubReleaseFile
          { owner = "dixslyf",
            repo = "unet-nuclei-instance-segmentation",
            tag = "v1.0.0",
            file = "unet-nuclei-instance-segmentation.ipynb"
          }

spec :: Spec
spec = around (withSystemTempDirectory "test-nbparts") $ do
  describe "Unpack then pack" $ do
    runSpecWithUnpackFormatsCA runTests
