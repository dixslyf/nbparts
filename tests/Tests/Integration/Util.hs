module Tests.Integration.Util where

import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Ipynb qualified as Ipynb
import Nbparts.Pack qualified as Nbparts
import Nbparts.Run (Command (Pack, Unpack), Options (Options))
import Nbparts.Run qualified as Nbparts
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack qualified as Nbparts

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
