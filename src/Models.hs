{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Models where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.Monad.Reader (ReaderT, asks, liftIO)
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool)
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Data.Int (Int64)

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Lesson json
  keyphrase String
  jsfile String
  title String
  goon String
  deriving Eq Show Generic
LessonPrereqs json
  lessonFor LessonId
  lessonRequired LessonId
  deriving Eq Show
User json
  name String
  email String
  UniqueUser email
  deriving Eq Show Generic
LessonCompleted json
  user LessonId
  lesson LessonId
  deriving Eq Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool
