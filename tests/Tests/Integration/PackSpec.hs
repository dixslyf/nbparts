module Tests.Integration.PackSpec where

import Control.Monad.Except (runExceptT)
import Data.Either qualified as Either
import Data.Maybe qualified as Maybe
import Nbparts.Pack (PackOptions (PackOptions, outputPath, partsDirectory))
import Nbparts.Types (NbpartsError)
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
import Nbparts.Unpack qualified as Unpack
import System.Directory qualified as Directory
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
    runPack,
    runSpecWithUnpackFormatsCA,
    runUnpack,
  )

data PackTestOptions = PackTestOptions
  { unpackOutputPath :: Maybe FilePath,
    packOutputPath :: Maybe FilePath
  }

testPackWith ::
  UnpackFormats ->
  PackTestOptions ->
  FilePath ->
  (Either NbpartsError () -> Expectation) ->
  FilePath ->
  Expectation
testPackWith
  (UnpackFormats {sourcesFormat, metadataFormat, outputsFormat})
  ( PackTestOptions
      { unpackOutputPath = relUnpackOutputPath,
        packOutputPath = relPackOutputPath
      }
    )
  fixture
  predicate
  tmpdir = do
    let nbPath = fixtureDir </> fixture
        unpackOutputPath = (</>) tmpdir <$> relUnpackOutputPath

    unpackResult <-
      runExceptT $
        runUnpack $
          UnpackOptions
            { notebookPath = nbPath,
              sourcesFormat,
              metadataFormat,
              outputsFormat,
              outputPath = unpackOutputPath
            }
    unpackResult `shouldSatisfy` Either.isRight

    packResult <-
      runExceptT $
        runPack $
          PackOptions
            { partsDirectory = Maybe.fromMaybe (Unpack.mkDefOutputPath nbPath) unpackOutputPath,
              outputPath = (</>) tmpdir <$> relPackOutputPath
            }
    predicate packResult

runTests :: UnpackFormats -> SpecWith FilePath
runTests unpackFormats = do
  let testPack = testPackWith unpackFormats
      testDefaultOutputPath unpackOutputPath expectedPackOutputPath tmpdir = do
        let packTestOpts =
              PackTestOptions
                { unpackOutputPath = Just unpackOutputPath,
                  packOutputPath = Nothing
                }
        testPack
          packTestOpts
          "empty.ipynb" -- Doesn't really matter what notebook we use.
          (`shouldSatisfy` Either.isRight)
          tmpdir
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

spec :: Spec
spec = around (withSystemTempDirectory "test-nbparts") $ do
  describe "Pack" $ do
    runSpecWithUnpackFormatsCA runTests
