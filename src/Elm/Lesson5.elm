module Lesson5 where

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
    validateX : List Int -> Int -> Maybe Bool
    validateX nums xguess =
      case nums of
        x :: y :: _ -> Just (x + y == xguess)
        _ -> Nothing
    validateY : List Int -> Int -> Maybe Bool
    validateY nums yguess =
      case nums of
        x :: y :: z :: _ -> Just ( x + y + yguess == z )
        _ -> Nothing
    numTriads = [ [2,3,9], [5,7,21], [40,1,42] ]

    triadToLine l =
      case l of
        (x :: y :: z :: _) -> [ [ (mkConstant x, red),  (mkConstant y, green) ]
                              , [ (mkVar "x", blue) ]
                              , [ (mkVar "x", blue), (mkVar "y", brown) ]
                              , [ (mkConstant z, purple) ]
                              ]
        _ -> [ ]
    numberlines = List.map (\ np -> NumberLine.init
                                    { height = 10, width = 350 }
                                    (triadToLine np)
                           ) numTriads
    questions = mkQBatch [ ("x", validateX), ("y", validateY) ] numTriads
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

    -- case action of
    --   SendCompletion -> ({model | completed = True}, Effects.none)
    --   Submission qid bid mval ->
    --     case mval of
    --       Nothing -> (model, Effects.none)
    --       Just val ->
    --           let
    --             updatedQuestions = List.map (updateQuestion ( qid, bid ) val) model.questions
    --             newestCompletion = findLatestAnswered updatedQuestions

    --             completion = newestCompletion + 1 > List.length model.questions

    --             completionAction = if completion
    --                                then
    --                                  Signal.send completed.address True
    --                                    |> Task.map (always SendCompletion)
    --                                    |> Effects.task
    --                                else Effects.none
    --           in
    --             ( { model | questions = updatedQuestions, qAt = newestCompletion + 1 }
    --             , completionAction
    --             )

-- View

targetToSubmission : Signal.Address Action -> (Maybe Int -> Action) -> String -> Signal.Message
targetToSubmission address f val =
  let
    mNumVal = val |> String.toInt |> Result.toMaybe
  in
    Signal.message address (f mNumVal)

viewQuestion : Signal.Address Action -> QuestionState -> Html
viewQuestion address {question,numberline} =
  let
    -- (n1, n2, n3) = case question.nums of
    --                  x :: y :: z :: _ -> (x, y, z)
    --                  _ -> (0, 0, 0)
    -- (g1, g2) = case question.boxes of
    --        (_, box1) :: (_, box2) :: _ -> (box1.guess, box2.guess)
    --        _ -> (Nothing, Nothing)
    (s1, s2, s3) = case question.nums of
                     x :: y :: z :: _ -> (toString x, toString y, toString z)
                     _ -> ("", "", "")
  in
    Html.div
          [ Html.Attributes.class "question" ]
          [ Html.div
            [ Html.Attributes.class "blanks" ]
            [ Html.div
                    []
                    [ makeMath (s1 ++ " + " ++ s2 ++ " = x") ]
            , Html.div
                    []
                    [ makeMath ("x + y = " ++ s3) ]
            , Html.div
                    []
                    [ makeMath "x = "
                    , Html.input
                            [ Html.Attributes.type' "text"
                            , completionClass question "x" |> Html.Attributes.class
                            , Html.Events.on "change"
                                  Html.Events.targetValue
                                  (targetToSubmission address (\x -> Submission (question.id, "x") x))
                            ]
                            [ ]
                    , Html.button
                            [ "btn btn-side " ++ completionClass question "x" |> Html.Attributes.class ]
                            [ Html.node "i"
                                    [ Html.Attributes.class (faClass question "x") ]
                                    [ ]
                            ]
                    ]
            , Html.div
                    []
                    [ makeMath "y = "
                    , Html.input
                            [ Html.Attributes.type' "text"
                            , completionClass question "y" |> Html.Attributes.class
                            , Html.Events.on "change"
                                  Html.Events.targetValue
                                  (targetToSubmission address (\y -> Submission (question.id, "y") y))
                            ]
                            [ ]
                    , Html.button
                            [ "btn btn-side btn-inverse " ++ completionClass question "y" |> Html.Attributes.class ]
                            [ Html.node "i"
                                    [ Html.Attributes.class (faClass question "y") ]
                                    [ ]
                            ]
                    ]
            ]
          , NumberLine.view (Signal.forwardTo address (RelayNumberLine question.id)) numberline
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
