{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
--import Database.Persist.Postgresql (selectList, Entity(..), (==.), fromSqlKey, toSqlKey, insert)
--import Data.Int (Int64)
import Servant

import Config (Config(..))
--import Models
import Api.User
import Api.Lesson
import Api.App
import Api.Page

type HandledAPI = "users" :> UserAPI
                  :<|> "lessons" :> LessonAPI
                  :<|> "page" :> PageAPI

handledAPI :: Proxy HandledAPI
handledAPI = Proxy

type WholeAPI = HandledAPI :<|> Raw

wholeAPI :: Proxy WholeAPI
wholeAPI = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> ServerT HandledAPI AppM -> Server WholeAPI
readerServer cfg server = (enter (readerToEither cfg) server)
                          :<|> serveDirectory "static"

app :: Config -> Application
app cfg = serve wholeAPI (readerServer cfg fullServer)

fullServer :: ServerT HandledAPI AppM
fullServer = userServer :<|> lessonServer :<|> pageServer
