{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Page where

import Control.Monad.Reader (lift)
import Control.Monad.Trans.Either (left)
import Database.Persist.Postgresql (selectList, Entity(..), (==.), toSqlKey)
import Data.Int (Int64)
import Lucid
import Servant
import Servant.HTML.Lucid
import Data.Char (toUpper)
import Data.List.Split (splitOn)

import Models
import Api.App

data Page = Page
            { number :: Int64
            , lesson :: Lesson
            }

instance ToHtml Page where
  toHtml (Page num les) =
    div_ $ do
           with section_ [class_ $ toText "jumbotron"] $ do
                  with div_ [class_ $ toText "keyphrase"] (h2_ (toHtml $ lessonKeyphrase les))
                  with div_ [id_ $ toText "elmApp"] (toHtml "")
                  with div_ [class_ $ toText "completion-div", id_ $ toText "completionDiv" ] (toHtml "")
           script_ []
             ("$(function() { startApp("++ show eqs ++");loadNextFiles("++ show num ++"); });")
    where systems = splitOn "," $ lessonEquations les
          eqs = map (splitOn ";") systems
  toHtmlRaw = toHtml

capitalize :: String -> String
capitalize "" = ""
capitalize (a:as) = toUpper a : as

pageAPI :: Proxy PageAPI
pageAPI = Proxy

type PageAPI =
  Capture "pageNum" Int64 :> Get '[HTML] Page
--  :<|> ReqBody '[JSON] Lesson :> Post '[JSON] Int64
  -- Eventually, post pages too GGto create

pageServer :: ServerT PageAPI AppM
pageServer = getPage

getPage :: Int64 -> AppM Page
getPage lessonId = do
  les <- runDb $ selectList [LessonId ==. toSqlKey lessonId] []
  case les of
    [] -> lift $ left err404
    (Entity _ x:_) -> return $ Page lessonId x
