module Lesson1 where

import Lesson exposing (Lesson, startLesson)
import Equation exposing (VisualType(..))
import Color exposing (..)
import Signal exposing (Signal)
import Html exposing (Html)
import Task exposing (Task)
import Effects exposing (Never)
import StartApp

equations : List (List String)
equations = [ ["2 + 3 = x"]
            , ["5 + 7 = x"]
            , ["2 + 40 = x"]
            ]

colors : List Color
colors = [ red, green, blue ]

app : StartApp.App Lesson
app = startLesson equations colors NumberLine

port tasks : Signal (Task Never ())
port tasks = app.tasks

main : Signal Html
main = app.html
