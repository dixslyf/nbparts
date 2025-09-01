module Tests.Integration.Util where

import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Ipynb qualified as Ipynb
import Data.Text qualified as Text
import Nbparts.Pack qualified as Nbparts
import Nbparts.Run (Command (Pack, Unpack), Options (Options))
import Nbparts.Run qualified as Nbparts
import Nbparts.Types qualified as Nbparts
import Nbparts.Types.Error (renderFormat)
import Nbparts.Unpack qualified as Nbparts
import Test.Hspec (SpecWith, context)

fixtureDir :: FilePath
fixtureDir = "tests/fixtures"

runUnpack :: (MonadError Nbparts.Error m, MonadIO m) => Nbparts.UnpackOptions -> m ()
runUnpack = Nbparts.run . Options . Unpack

runPack :: (MonadError Nbparts.Error m, MonadIO m) => Nbparts.PackOptions -> m ()
runPack = Nbparts.run . Options . Pack

readIpynb :: (MonadError String m, MonadIO m) => FilePath -> m (Ipynb.Notebook Ipynb.NbV4)
readIpynb fp = do
  bytes <- liftIO $ ByteString.Lazy.readFile fp
  liftEither $ Aeson.eitherDecode bytes

data UnpackFormats = UnpackFormats
  { sourcesFormat :: Nbparts.Format,
    metadataFormat :: Nbparts.Format,
    outputsFormat :: Nbparts.Format
  }

runSpecWithUnpackFormatsCA :: (UnpackFormats -> SpecWith a) -> SpecWith a
runSpecWithUnpackFormatsCA spec = mapM_ (runSpecWithUnpackFormats spec) unpackFormatsCA

runSpecWithUnpackFormats :: (UnpackFormats -> SpecWith a) -> UnpackFormats -> SpecWith a
runSpecWithUnpackFormats spec fmts@(UnpackFormats {sourcesFormat, metadataFormat, outputsFormat}) =
  context
    ( "when exporting sources to "
        <> Text.unpack (renderFormat sourcesFormat)
        <> ", metadata to "
        <> Text.unpack (renderFormat metadataFormat)
        <> " and outputs to "
        <> Text.unpack (renderFormat outputsFormat)
    )
    $ spec fmts

-- Combinations generated with CAgen.
unpackFormatsCA :: [UnpackFormats]
unpackFormatsCA =
  [ UnpackFormats
      { sourcesFormat = Nbparts.FormatYaml,
        metadataFormat = Nbparts.FormatYaml,
        outputsFormat = Nbparts.FormatYaml
      },
    UnpackFormats
      { sourcesFormat = Nbparts.FormatYaml,
        metadataFormat = Nbparts.FormatJson,
        outputsFormat = Nbparts.FormatJson
      },
    UnpackFormats
      { sourcesFormat = Nbparts.FormatJson,
        metadataFormat = Nbparts.FormatYaml,
        outputsFormat = Nbparts.FormatJson
      },
    UnpackFormats
      { sourcesFormat = Nbparts.FormatJson,
        metadataFormat = Nbparts.FormatJson,
        outputsFormat = Nbparts.FormatYaml
      },
    UnpackFormats
      { sourcesFormat = Nbparts.FormatMarkdown,
        metadataFormat = Nbparts.FormatYaml,
        outputsFormat = Nbparts.FormatJson
      },
    UnpackFormats
      { sourcesFormat = Nbparts.FormatMarkdown,
        metadataFormat = Nbparts.FormatJson,
        outputsFormat = Nbparts.FormatYaml
      }
  ]
