module Lesson4 where

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

-- Model

type alias ID = Int

type alias Model =
  { questions : List Question
  , qAt : ID
  , completed : Bool
  }

type alias Question =
  { num11 : Int
  , num12 : Int
  , num21 : Int
  , attempted : Bool
  , completed : Bool
  , id : ID         
  }

mkQuestion : Int -> Int -> Int -> ID -> Question
mkQuestion x y z id = { num11 = x
                         , num12 = y
                         , num21 = z
                         , attempted = False
                         , completed = False
                         , id = id
                         }

init : Model
init = { questions = [ mkQuestion 2 3 4 1 
                      , mkQuestion 5 7 9 2
                      , mkQuestion 40 1 1 3
                      ]
        , qAt = 1
        , completed = False 
        }

-- Update

type Action = Submission ID (Maybe Int)
            | SendCompletion

port signalCompletion : Signal Bool
port signalCompletion = completed.signal

completed : Signal.Mailbox Bool
completed = Signal.mailbox False

updateQuestion : ID -> Int -> Question -> Question
updateQuestion id val question =
  if question.id == id
  then
    let
      completed = question.num11 + question.num12 == val
    in
      { question | attempted <- True, completed <- completed }
  else
    question
            
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SendCompletion -> ({model | completed <- True}, Effects.none)
    Submission id mval ->
      case mval of
        Nothing -> (model, Effects.none)
        Just val ->
          let            
            updatedQuestions = List.map (updateQuestion id val) model.questions

            newestCompletion = updatedQuestions
                             |> List.filter (.completed)
                             |> List.map (.id)
                             |> List.maximum
                             |> Maybe.withDefault 0
                                
            completion = newestCompletion + 1 > List.length model.questions

            completionAction = if completion
                               then
                                 Signal.send completed.address True
                                   |> Task.map (always SendCompletion)
                                   |> Effects.task
                               else Effects.none
      in
        ( { model | questions <- updatedQuestions, qAt <- newestCompletion + 1 }
        , completionAction
        )

-- View

targetToSubmission : Signal.Address Action -> ID -> String -> Signal.Message
targetToSubmission address id val =
  let
    mNumVal = val |> String.toInt |> Result.toMaybe
  in
    Signal.message address (Submission id mNumVal)

completionClass : Question -> Attribute
completionClass question =
  if question.completed
  then
    Html.Attributes.class "completed"
  else
    if question.attempted
    then
      Html.Attributes.class "incorrect"
    else
      Html.Attributes.class "new-question"
    
viewQuestion : Signal.Address Action -> Question -> Html
viewQuestion address question =
  Html.div
      [ Html.Attributes.class "question" ]
      [ Html.div
            []
            [ Html.text (toString question.num11)
            , Html.text " + "
            , Html.text (toString question.num12)
            , Html.text " = x"

            ]
      , Html.div
            []
            [ Html.text "x + "
            , Html.text (toString question.num21)
            , Html.text " = "
            , Html.text (toString (question.num11 + question.num12 + question.num21))
            ]
      , Html.div
            []
            [ Html.text "x = "
            , Html.input
              [ Html.Attributes.type' "text"
              , completionClass question
              , Html.Events.on "change" (Html.Events.targetValue) (targetToSubmission address question.id)
              ]
              [ ]
            ]
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
