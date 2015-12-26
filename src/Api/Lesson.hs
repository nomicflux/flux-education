{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Lesson where

import           Control.Monad.Reader        (ReaderT, lift, runReaderT)
import           Control.Monad.Trans.Either  (EitherT, left)
import           Data.Int                    (Int64)
import           Database.Esqueleto          ((^.))
import qualified Database.Esqueleto          as E
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectList, toSqlKey, (==.))
import           Servant

import           Api.App
import           Models

lessonAPI :: Proxy LessonAPI
lessonAPI = Proxy

type LessonAPI =
  Get '[JSON] [Lesson]
  :<|> Capture "lesson" Int64 :> Get '[JSON] Lesson
  :<|> "requires" :> Capture "lesson" Int64 :> Get '[JSON] [(Int64, Lesson)]
  :<|> "requiredby" :> Capture "lesson" Int64 :> Get '[JSON] [(Int64, Lesson)]
  :<|> "completed" :> Capture "user" Int64 :> Get '[JSON] [(Int64, Lesson)]
  :<|> ReqBody '[JSON] Lesson :> Post '[JSON] Int64

lessonServer :: ServerT LessonAPI AppM
lessonServer = allLessons
               :<|> singleLesson
               :<|> requiresLessons
               :<|> requiredByLessons
               :<|> completedLessons
               :<|> createLesson

allLessons :: AppM [Lesson]
allLessons = do
  lessons <- runDb $ selectList [] []
  justLessons <- return $ map (\(Entity _ x) -> x) lessons
  return justLessons

singleLesson :: Int64 -> AppM Lesson
singleLesson key = do
  mlesson <- runDb $ selectList [LessonId ==. (toSqlKey key)] []
  case mlesson of
   [] -> lift $ left err404
   ((Entity _ x):xs) -> return x

requiresLessons :: Int64 -> AppM [(Int64, Lesson)]
requiresLessons key = do
  lessons <- runDb
             $ E.select
             $ E.from $ \(E.InnerJoin lessonPrereqs lesson) -> do
               E.where_ (lessonPrereqs ^. LessonPrereqsLessonFor E.==. E.val (toSqlKey key))
               E.on $ lessonPrereqs ^. LessonPrereqsLessonRequired E.==. lesson ^. LessonId
               return lesson
  justLessons <- return $ map (\(Entity lid l) -> (fromSqlKey lid, l)) lessons
  return justLessons

requiredByLessons :: Int64 -> AppM [(Int64, Lesson)]
requiredByLessons key = do
  lessons <- runDb
             $ E.select
             $ E.from $ \(E.InnerJoin lessonPrereqs lesson) -> do
               E.where_ (lessonPrereqs ^. LessonPrereqsLessonRequired E.==. E.val (toSqlKey key))
               E.on $ lessonPrereqs ^. LessonPrereqsLessonFor E.==. lesson ^. LessonId
               return lesson
  justLessons <- return $ map (\(Entity lid l) -> (fromSqlKey lid, l)) lessons
  return justLessons

completedLessons :: Int64 -> AppM [(Int64, Lesson)]
completedLessons key = do
  lessons <- runDb
             $ E.select
             $ E.from $ \(E.InnerJoin lesson lessonCompleted) -> do
               E.where_ $ lessonCompleted ^. LessonCompletedUser E.==. E.val (toSqlKey key)
               E.on $ lessonCompleted ^. LessonCompletedLesson E.==. lesson ^. LessonId
               return lesson
  justLessons <- return $ map (\(Entity lid l) -> (fromSqlKey lid, l)) lessons
  return justLessons

createLesson :: Lesson -> AppM Int64
createLesson lesson = do
  newLesson <- runDb $ insert lesson
  return $ fromSqlKey newLesson
