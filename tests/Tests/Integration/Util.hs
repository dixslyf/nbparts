module Tests.Integration.Util where

import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Ipynb qualified as Ipynb
import Data.Text qualified as Text
import Nbparts.Pack (PackOptions)
import Nbparts.Run (Command (Pack, Unpack), Options (Options))
import Nbparts.Run qualified as Nbparts
import Nbparts.Types
  ( Format (FormatJson, FormatMarkdown, FormatYaml),
    NbpartsError,
    renderFormat,
  )
import Nbparts.Unpack (UnpackOptions)
import Test.Hspec (SpecWith, context)

fixtureDir :: FilePath
fixtureDir = "tests/fixtures"

runUnpack :: (MonadError NbpartsError m, MonadIO m) => UnpackOptions -> m ()
runUnpack = Nbparts.run . Options . Unpack

runPack :: (MonadError NbpartsError m, MonadIO m) => PackOptions -> m ()
runPack = Nbparts.run . Options . Pack

readIpynb :: (MonadError String m, MonadIO m) => FilePath -> m (Ipynb.Notebook Ipynb.NbV4)
readIpynb fp = do
  bytes <- liftIO $ ByteString.Lazy.readFile fp
  liftEither $ Aeson.eitherDecode bytes

data UnpackFormats = UnpackFormats
  { sourcesFormat :: Format,
    metadataFormat :: Format,
    outputsFormat :: Format
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
      { sourcesFormat = FormatYaml,
        metadataFormat = FormatYaml,
        outputsFormat = FormatYaml
      },
    UnpackFormats
      { sourcesFormat = FormatYaml,
        metadataFormat = FormatJson,
        outputsFormat = FormatJson
      },
    UnpackFormats
      { sourcesFormat = FormatJson,
        metadataFormat = FormatYaml,
        outputsFormat = FormatJson
      },
    UnpackFormats
      { sourcesFormat = FormatJson,
        metadataFormat = FormatJson,
        outputsFormat = FormatYaml
      },
    UnpackFormats
      { sourcesFormat = FormatMarkdown,
        metadataFormat = FormatYaml,
        outputsFormat = FormatJson
      },
    UnpackFormats
      { sourcesFormat = FormatMarkdown,
        metadataFormat = FormatJson,
        outputsFormat = FormatYaml
      }
  ]
