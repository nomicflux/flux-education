{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Api where

import Control.Monad (liftM)
import Control.Monad.Reader (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either (EitherT, left)
import Network.Wai (Application)
import Database.Persist.Postgresql (selectList, Entity(..), (==.), fromSqlKey, insert)
import Data.Int (Int64)
import Servant

import Config (Config(..))
import Models

type LessonAPI =
  Get '[JSON] [Lesson]
  :<|> Capture "lesson" Int64 :> Get '[JSON] Lesson
  :<|> "required" :> Capture "lesson" Int64 :> Get '[JSON] [Lesson]
  :<|> "requiredby" :> Capture "lesson" Int64 :> Get '[JSON] [Lesson]
  :<|> "completed" :> Capture "user" Int64 :> Get '[JSON] [Lesson]
  :<|> ReqBody '[JSON] Lesson :> Post '[JSON] Int64

type UserAPI =
  Get '[JSON] [User]
  :<|> Capture "email" String :> Get '[JSON] User
  :<|> ReqBody '[JSON] User :> Post '[JSON] Int64

type AppM = ReaderT Config (EitherT ServantErr IO)

type FullAPI = "users" :> UserAPI
               :<|> "lessons" :> LessonAPI

lessonAPI :: Proxy LessonAPI
lessonAPI = Proxy

userAPI :: Proxy UserAPI
userAPI = Proxy

fullAPI :: Proxy FullAPI
fullAPI = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> ServerT FullAPI AppM -> Server FullAPI
readerServer cfg server = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve fullAPI (readerServer cfg fullServer)

userServer :: ServerT UserAPI AppM
userServer = allUsers
             :<|> singleUser
             :<|> createUser

lessonServer :: ServerT LessonAPI AppM
lessonServer = allLessons
               :<|> singleLesson
               :<|> requiredLessons
               :<|> requiredByLessons
               :<|> completedLessons
               :<|> createLesson

fullServer :: ServerT FullAPI AppM
fullServer = userServer :<|> lessonServer

allUsers :: AppM [User]
allUsers = do
  users <- runDb $ selectList [] []
  justUsers <- return $ map (\(Entity _ x) -> x) users
  return justUsers

singleUser :: String -> AppM User
singleUser str = do
  users <- runDb $ selectList [UserEmail ==. str] []
  case users of
   [] -> lift $ left err404
   ((Entity _ x):xs) -> return x

createUser :: User -> AppM Int64
createUser user = do
  newUser <- runDb $ insert user
  return $ fromSqlKey newUser

allLessons :: AppM [Lesson]
allLessons = do
  lessons <- runDb $ selectList [] []
  justLessons <- return $ map (\(Entity _ x) -> x) lessons
  return justLessons

singleLesson :: Int64 -> AppM Lesson
singleLesson = undefined

requiredLessons :: Int64 -> AppM [Lesson]
requiredLessons = undefined

requiredByLessons :: Int64 -> AppM [Lesson]
requiredByLessons = undefined

completedLessons :: Int64 -> AppM [Lesson]
completedLessons = undefined

createLesson :: Lesson -> AppM Int64
createLesson lesson = do
  newLesson <- runDb $ insert lesson
  return $ fromSqlKey newLesson
