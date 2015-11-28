module Lesson5 where

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

type alias QuestionID = Int

type alias BoxID = Int

type alias InputBox =
  { completed : Bool
  , attempted : Bool
  }
                   
type alias Model =
  { phrase : String
  , questions : List Question
  , qAt : QuestionID
  }

type alias Question =
  { num11 : Int
  , num12 : Int
  , num22 : Int
  , boxes : List ( BoxID, InputBox )
  , id : QuestionID         
  }

mkBox : BoxID -> (BoxID, InputBox)
mkBox bid = (bid,
                  { completed = False
                  , attempted = False
                  }
              )
                    
mkQuestion : Int -> Int -> Int -> QuestionID -> Question
mkQuestion x y z id = { num11 = x
                         , num12 = y
                         , num22 = z
                         , boxes = [ mkBox 1, mkBox 2 ]
                         , id = id
                         }

init : Model
init = { phrase = "Some questions must be answered together."
        , questions = [ mkQuestion 2 3 9 1 
                      , mkQuestion 5 7 21 2
                      , mkQuestion 40 1 42 3
                      ]
        , qAt = 1
        }

-- Update

type Action = Submission QuestionID BoxID (Maybe Int)

updateBox : BoxID -> Bool -> ( BoxID, InputBox ) -> ( BoxID, InputBox )
updateBox wantedBid completion (thisBid, box) =
  if thisBid == wantedBid
  then
    (thisBid, { box | attempted <- True, completed <- completion })
  else
    (thisBid, box)
            
updateQuestion : Int -> ( QuestionID, BoxID ) -> Question -> Question
updateQuestion val (qid, bid) question =
  if question.id == qid
  then
    let
      completionVals = [ question.num11 + question.num12 == val
                       , question.num22 - question.num11 - question.num12 == val
                       ]
      updatedBoxes = List.map2 (updateBox bid) completionVals question.boxes
    in
      { question | boxes <- updatedBoxes }
  else
    question

boxesComplete : Question -> Bool
boxesComplete q = List.all (\ (_, b) -> b.completed ) q.boxes
    
findLatestAnswered : List Question -> QuestionID
findLatestAnswered qs = qs
                      |> List.filter boxesComplete
                      |> List.map (.id)
                      |> List.maximum
                      |> Maybe.withDefault 0

update : Action -> Model -> Model
update action model =
    case action of
      Submission qid bid mval ->
        case mval of
          Nothing -> model
          Just val ->
              let
                updatedQuestions = List.map (updateQuestion val ( qid, bid )) model.questions
                newestCompletion = findLatestAnswered updatedQuestions
              in
                { model | questions <- updatedQuestions, qAt <- newestCompletion + 1 }

-- View

targetToSubmission : Signal.Address Action -> (Maybe Int -> Action) -> String -> Signal.Message
targetToSubmission address f val =
  let
    mNumVal = val |> String.toInt |> Result.toMaybe
  in
    Signal.message address (f mNumVal)

completionClass : Question -> BoxID -> Attribute
completionClass question bid =
  let
    mbox = List.head (List.filter (\ (id, _) -> id == bid) question.boxes)
  in
    case mbox of
      Nothing -> Html.Attributes.class "none"
      Just (_, box) ->
        if box.completed
        then
          Html.Attributes.class "completed"
        else
          if box.attempted
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
            [ Html.math [] [ Html.text ((toString question.num11) ++ " + " ++ (toString question.num12) ++ " = x") ]
            ]
      , Html.div
            []
            [ Html.math [] [ Html.text ("x + y = " ++ (toString question.num22)) ]
            ]
      , Html.div
            []
            [ Html.math [] [ Html.text "x = " ]
            , Html.input
              [ Html.Attributes.type' "text"
              , completionClass question 1
              , Html.Events.on "change" (Html.Events.targetValue)
                      (targetToSubmission address (\x -> Submission question.id 1 x))
              ]
              [ ]
            ]
      , Html.div
            []
            [ Html.math [] [ Html.text "y = " ]
            , Html.input
              [ Html.Attributes.type' "text"
              , completionClass question 2
              , Html.Events.on "change" (Html.Events.targetValue)
                      (targetToSubmission address (\y -> Submission question.id 2 y))
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
                      [ Html.Attributes.href "Lesson6.html" ]
                      [ Html.text "Go to Lesson 6" ]
              ]
      else
        Html.div
            [ Html.Attributes.class "noncompletion completion" ]
            [ Html.text "Complete all questions before moving on" ]
  in
    Html.section
          [ Html.Attributes.class "jumbotron lesson5" ]
          ( [ Html.h1 [] [ Html.text "Lesson 5" ]
            , Html.h2 [ Html.Attributes.class "keyphrase" ] [ Html.text model.phrase ]
            ] ++ (model.questions
                    |> List.map (viewQuestion address)
                    |> List.take model.qAt ) ++ [ completionDiv ] )

-- All Together

main = StartApp.Simple.start { model = init
                             , update = update
                             , view   = view
                             }
