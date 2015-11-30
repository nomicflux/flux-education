module Api.App where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Either (EitherT)
import Servant (ServantErr)

import Config (Config(..))

type AppM = ReaderT Config (EitherT ServantErr IO)
