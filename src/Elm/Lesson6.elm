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

type alias QuestionState = { question : Question
                           , numberline : NumberLine.State
                           }

type alias Model =
  { questions : List QuestionState
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
    pairToLine l =
      case l of
        (x :: y :: _) -> [ [ (Variable {name="x", value=Nothing, prevValue=Nothing}, blue), (Constant {value=x}, red) ]
                         , [ (Constant {value=y}, green) ]
                         , []
                         , [ (Constant {value=y}, green), (Constant {value=(-x)}, lightCharcoal) ]
                         , [ (Variable {name="x", value=Nothing, prevValue=Nothing}, blue) ]
                         ]
        _ -> [ ]
    numberlines = List.map (\ np -> NumberLine.init
                                    { height = 10, width = 350 }
                                    (pairToLine np)
                           ) numPairs
    questions = mkQBatch [ ("x", validate) ] numPairs

  in
    { questions = List.map2 (\ q nl -> {question=q, numberline=nl}) questions numberlines
    , qAt = 1
    , completed = False
    }

-- Update

type Action = Submission (QuestionID, BoxID) (Maybe Int)
            | RelayNumberLine QuestionID NumberLine.Action
            | SendCompletion

port signalCompletion : Signal Bool
port signalCompletion = completed.signal

completed : Signal.Mailbox Bool
completed = Signal.mailbox False

updateNumberLines : ( Question ->  Question )
                  -> ( NumberLine.State -> ( NumberLine.State, Effects NumberLine.Action ) )
                  -> QuestionID
                  -> List QuestionState
                  -> (List QuestionState, List (Effects Action))
updateNumberLines qFunc nlFunc wanted pairs =
  let
    updateBoth {question, numberline} =
      if question.id == wanted
      then
        let
          (newNL, nlEffect) = nlFunc numberline
        in
          ({question=qFunc question, numberline=newNL}, (question.id, nlEffect))
      else
        ({question=question, numberline=numberline}, (question.id, Effects.none))

    allUpdate = List.map updateBoth pairs
    updatedQuestions = List.map fst allUpdate
    updatedEffects = List.map (snd >> (\ (i, e) -> Effects.map (RelayNumberLine i) e)) allUpdate
  in
    ( updatedQuestions, updatedEffects )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SendCompletion -> ({model | completed = True}, Effects.none)
    Submission (qid, bid) mval ->
      case mval of
        Nothing -> (model, Effects.none)
        Just val ->
          let
            updateThisQ = updateQuestion (qid, bid) val
            updateThisV s = NumberLine.update s (NumberLine.UpdateVariable bid val)

            (qs, es) = updateNumberLines updateThisQ updateThisV qid model.questions

            newestCompletion = findLatestAnswered (List.map (.question) qs)
            completion = newestCompletion + 1 > List.length model.questions

            completionAction = if completion
                               then
                                 Signal.send completed.address True
                                   |> Task.map (always SendCompletion)
                                   |> Effects.task
                               else Effects.none
          in
            ( { model | questions = qs, qAt = newestCompletion + 1 }
            , Effects.batch (completionAction :: es)
            )
    RelayNumberLine qid nlEffect ->
      let
        fireEffect numberline = NumberLine.update numberline nlEffect

        (qs, es) = updateNumberLines identity fireEffect qid model.questions
      in
        ( { model | questions = qs }, Effects.batch es )

-- View

targetToSubmission : Signal.Address Action -> QuestionID -> BoxID -> String -> Signal.Message
targetToSubmission address qid bid val =
  let
    mNumVal = val |> String.toInt |> Result.toMaybe
  in
    Signal.message address (Submission (qid, bid) mNumVal)

viewQuestion : Signal.Address Action -> QuestionState -> Html
viewQuestion address {question,numberline} =
  let
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
                                , completionClass question "x" |> Html.Attributes.class
                                , Html.Events.on "change"
                                      Html.Events.targetValue
                                            (targetToSubmission address question.id "x")
                                ]
                                [ ]
                        , Html.button
                                [ "btn btn-side " ++ completionClass question "x" |> Html.Attributes.class ]
                                [ Html.node "i"
                                        [ Html.Attributes.class (faClass question "x") ]
                                        [ ]
                                ]
                        ]
                ]
          , NumberLine.view (Signal.forwardTo address (RelayNumberLine question.id)) numberline
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
