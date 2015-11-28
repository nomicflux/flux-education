module Lesson2 where

import Signal exposing (Signal)
import List
import Maybe exposing (Maybe)
import Result
import String 
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import StartApp.Simple

-- Model

type alias ID = Int

type alias Model =
  { phrase : String
  , questions : List Question
  , qAt : ID
  }

type alias Question =
  { num1 : Int
  , num2 : Int
  , attempted : Bool
  , completed : Bool
  , id : ID         
  }

mkQuestion : Int -> Int -> ID -> Question
mkQuestion x y id = { num1 = x
                       , num2 = y
                       , attempted = False
                       , completed = False
                       , id = id
                       }

init : Model
init = { phrase = "An X doesn't change anything."
        , questions = [ mkQuestion 2 3 1 
                      , mkQuestion 5 7 2
                      , mkQuestion 40 2 3
                      ]
        , qAt = 1
        }

-- Update

type Action = Submission ID (Maybe Int)

update : Action -> Model -> Model
update (Submission id mval) model =
  case mval of
    Nothing -> model
    Just val ->
      let
        updateQuestion : Question -> Question
        updateQuestion question =
          if question.id == id
          then
            let
              completed = question.num1 + question.num2 == val
            in
              { question | attempted <- True, completed <- completed }
          else
            question
            
        updatedQuestions = List.map updateQuestion model.questions

        newestCompletion = updatedQuestions
                             |> List.filter (.completed)
                             |> List.map (.id)
                             |> List.maximum
                             |> Maybe.withDefault 0
      in
        { model | questions <- updatedQuestions, qAt <- newestCompletion + 1 }

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
            [ Html.text (toString question.num1)
            , Html.text " + "
            , Html.text (toString question.num2)
            , Html.text " = x"
            ]
      , Html.div
            []
            [ Html.text "x  = "
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
  let
    completion = model.qAt > List.length model.questions
    completionDiv =
      if completion
      then
        Html.div
              [ Html.Attributes.class "completion" ]
              [ Html.a
                      [ Html.Attributes.href "Lesson3.html" ]
                      [ Html.text "Go to Lesson 3" ]
              ]
      else
        Html.div
            [ Html.Attributes.class "noncompletion completion" ]
            [ Html.text "Complete all questions before moving on" ]
  in
    Html.section
          [ Html.Attributes.class "jumbotron lesson2" ]
          ( [ Html.h1 [] [ Html.text "Lesson 2" ]
            , Html.h2 [ Html.Attributes.class "keyphrase" ] [ Html.text model.phrase ]
            ] ++ (model.questions
                    |> List.map (viewQuestion address)
                    |> List.take model.qAt ) ++ [ completionDiv ] )

-- All Together

main = StartApp.Simple.start { model = init
                             , update = update
                             , view   = view
                             }
