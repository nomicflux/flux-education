{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Page where

import Control.Monad.Reader (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either (EitherT, left)
import Database.Persist.Postgresql (selectList, Entity(..), (==.), fromSqlKey, toSqlKey, insert)
import Data.Int (Int64)
import Lucid
import Servant
import Servant.HTML.Lucid

import Models
import Api.App

data Page = Page
            { number :: Int64
            , lesson :: Lesson
            }

instance ToHtml Page where
  toHtml (Page num lesson) =
    html_ $ do
      head_ $ do
        title_ (toHtml $ "Flux")
        (script_ [src_ (toText $ "/elm/" ++ (lessonJsfile lesson) ++ ".js")] "")
        link_ [href_ $ toText "/css/bootstrap.css", type_ $ toText "text/css", rel_ $ toText "stylesheet"]
        link_ [href_ $ toText "/css/lessons.css", type_ $ toText "text/css", rel_ $ toText "stylesheet"]
      body_ $ do
        with section_ [class_ $ toText "jumbotron"] $ do
          h1_ (toHtml $ lessonTitle lesson)
          h2_ (toHtml $ lessonKeyphrase lesson)
          with div_ [id_ (toText "elmApp")] (toHtml "")
        (script_ [] "var elmApp = document.getElementById(\"elmApp\"); Elm.embed(Elm.Lesson1, elmApp);")
  toHtmlRaw = toHtml

pageAPI :: Proxy PageAPI
pageAPI = Proxy

type PageAPI =
  Capture "pageNum" Int64 :> Get '[HTML] Page
--  :<|> ReqBody '[JSON] Lesson :> Post '[JSON] Int64
  -- Eventually, post pages too to create

pageServer :: ServerT PageAPI AppM
pageServer = getPage

getPage :: Int64 -> AppM Page
getPage lessonId = do
  lesson <- runDb $ selectList [LessonId ==. (toSqlKey lessonId)] []
  case lesson of
    [] -> lift $ left err404
    ((Entity _ x):xs) -> return $ Page lessonId x
