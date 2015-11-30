{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad.Reader (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either (EitherT, left)
import Database.Persist.Postgresql (selectList, Entity(..), (==.), fromSqlKey, toSqlKey, insert)
import Data.Int (Int64)
import Servant

import Models
import Api.App

type UserAPI =
  Get '[JSON] [User]
  :<|> Capture "email" String :> Get '[JSON] User
  :<|> ReqBody '[JSON] User :> Post '[JSON] Int64

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: ServerT UserAPI AppM
userServer = allUsers
             :<|> singleUser
             :<|> createUser

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
