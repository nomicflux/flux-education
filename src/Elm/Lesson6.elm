module Lesson6 where

import Signal exposing (Signal)
import List
import Maybe exposing (Maybe)
import Result
import String
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import StartApp
import Color exposing (..)
import NumberLine exposing (..)
import Question exposing (..)
import Effects exposing (Effects, Never)
import Task exposing (Task, andThen)

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
        x :: y :: _ -> Just ( x + guess == y )
        _ -> Nothing
    numPairs = [ [2,5], [5,12], [40,42] ]
  in
    { questions = mkQBatch [validate] numPairs
    , qAt = 1
    , completed = False
    }

-- Update

type Action = Submission QuestionID BoxID (Maybe Int)
            | SendCompletion

port signalCompletion : Signal Bool
port signalCompletion = completed.signal

completed : Signal.Mailbox Bool
completed = Signal.mailbox False

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
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
                                   |> Task.map (always SendCompletion)
                                   |> Effects.task
                               else Effects.none
      in
        ( { model | questions = updatedQuestions, qAt = newestCompletion + 1 }
        , completionAction
        )

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
                [ Html.div
                        []
                        [ makeMath (s1 ++ " + x = " ++ s2) ]
                , Html.div
                        []
                        [ makeMath ("     x = " ++ s2 ++ " - " ++ s1) ]
                , Html.div
                        []
                        [ Html.text "x  = "
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
                ]
          , Html.div
                  [ Html.Attributes.class "bars" ]
                  [ barCollage { height = 10, width = 350 }
                                 [ [ (n1, lightCharcoal), (Maybe.withDefault 0 g1, blue) ]
                                 , [ (n2, green) ]
                                 ] |> Html.fromElement ]
      ]

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div
        [ Html.Attributes.class "specificLesson"]
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
