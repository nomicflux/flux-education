{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad (liftM)
import Control.Monad.Reader (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either (EitherT, left)
import Network.Wai (Application)
import Database.Persist.Postgresql (selectList, Entity(..), (==.), fromSqlKey, toSqlKey, insert)
import Data.Int (Int64)
import Servant

import Config (Config(..))
import Models
import Api.User
import Api.Lesson
import Api.App

type FullAPI = "users" :> UserAPI
               :<|> "lessons" :> LessonAPI

fullAPI :: Proxy FullAPI
fullAPI = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> ServerT FullAPI AppM -> Server FullAPI
readerServer cfg server = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve fullAPI (readerServer cfg fullServer)

fullServer :: ServerT FullAPI AppM
fullServer = userServer :<|> lessonServer
