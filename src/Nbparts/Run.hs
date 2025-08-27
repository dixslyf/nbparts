module Nbparts.Run
  ( Options (..),
    Command (..),
    run,
  )
where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO)
import Nbparts.Pack (PackOptions)
import Nbparts.Pack qualified as Nbparts
import Nbparts.Types qualified as Nbparts
import Nbparts.Unpack (UnpackOptions)
import Nbparts.Unpack qualified as Nbparts

newtype Options = Options
  { command :: Command
  }

data Command = Unpack UnpackOptions | Pack PackOptions

run :: (MonadError Nbparts.Error m, MonadIO m) => Options -> m ()
run (Options command) = case command of
  Unpack unpackOpts -> hoistError Nbparts.UnpackError $ Nbparts.unpack unpackOpts
  Pack packOpts -> hoistError Nbparts.PackError $ Nbparts.pack packOpts

hoistError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
hoistError mapErr action = do
  res <- runExceptT $ withExceptT mapErr action
  case res of
    Right value -> pure value
    Left err -> throwError err
