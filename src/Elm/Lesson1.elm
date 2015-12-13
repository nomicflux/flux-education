module Lesson1 where

import Signal exposing (Signal)
import List
import Maybe exposing (Maybe)
import Result
import String
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task, andThen)
import Color exposing (..)
import NumberLine exposing (..)
import Question exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

-- Model

type alias Model =
  { questions : List Question
  , qAt : QuestionID
  , completed : Bool
  }

init : Model
init =
  let
    validate : List Int -> Int -> Maybe Bool
    validate nums guess =
      case nums of
        x :: y :: _ -> Just (x + y == guess)
        _ -> Nothing
    numPairs = [ [2,3], [5,7], [40,2] ]
  in
    { questions = mkQBatch [ validate ] numPairs
    , qAt = 1
    , completed = False
    }

-- Update

type Action = Submission QuestionID BoxID (Maybe Int)
            | SendCompletion
            | NoOp

port signalCompletion : Signal Bool
port signalCompletion = completed.signal

completed : Signal.Mailbox Bool
completed = Signal.mailbox False

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    SendCompletion -> ({model | completed = True}, Effects.none)
    Submission qid bid mval ->
      case mval of
        Nothing -> (model, Effects.none)
        Just val ->
          let
            updatedQuestions = List.map (updateQuestion (qid, bid) val) model.questions

            newestCompletion = findLatestAnswered updatedQuestions
            completion = newestCompletion + 1 > List.length model.questions

            completionAction = if completion
                               then
                                 Signal.send completed.address True
                                   |> taskToNone
                                   |> Effects.task
                               else Effects.none
          in
            ( { model | questions = updatedQuestions, qAt = newestCompletion + 1 }
            , completionAction
            )

taskToNone : Task Never () -> Task Never Action
taskToNone task = Task.map (always SendCompletion) task

-- View

targetToSubmission : Signal.Address Action -> QuestionID -> BoxID -> String -> Signal.Message
targetToSubmission address qid bid val =
  let
    mNumVal = val |> String.toInt |> Result.toMaybe
  in
    Signal.message address (Submission qid bid mNumVal)

viewQuestion : Signal.Address Action -> Question -> Html
viewQuestion address question =
  let
    (n1, n2) = case question.nums of
                 x :: y :: _ -> (x, y)
                 _ -> (0, 0)
    g1 = case question.boxes of
           (_, box) :: _ -> box.guess
           _ -> Nothing
    (s1, s2) = case question.nums of
                 x :: y :: _ -> (toString x, toString y)
                 _ -> ("", "")
  in
    Html.div
          [ Html.Attributes.class "question" ]
          [ Html.div
                  [ Html.Attributes.class "blanks" ]
                  [ makeMath (s1 ++ " + " ++ s2 ++ " = ")
                  , Html.input
                          [ Html.Attributes.type' "text"
                          , completionClass question 1 |> Html.Attributes.class
                          , Html.Events.on "change"
                                Html.Events.targetValue
                                (targetToSubmission address question.id 1)
                          ]
                          [ ]
                  , Html.button
                          [ "btn btn-side " ++ completionClass question 1 |> Html.Attributes.class ]
                          [ Html.node "i"
                                  [ Html.Attributes.class (faClass question 1) ]
                                  [ ]
                          ]
                  ]
          , Html.div
                  [ Html.Attributes.class "bars" ]
                  [ barCollage { height = 10, width = 350 }
                                 [ [ (n1, red), (n2, green) ]
                                 , [ (Maybe.withDefault 0 g1, blue) ]
                                 ] |> Html.fromElement ]
          ]

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div
        [ Html.Attributes.class "specificLesson" ]
        (model.questions
           |> List.map (viewQuestion address)
           |> List.take model.qAt )

-- All Together
app = StartApp.start
      { init = (init, Effects.none)
      , update = update
      , view   = view
      , inputs = []
      }

port tasks : Signal (Task Never ())
port tasks = app.tasks

main = app.html
