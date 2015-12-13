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

type alias Model =
  { questions : List Question
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
  in
    { questions = mkQBatch [validateX, validateY] numTriads
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
                updatedQuestions = List.map (updateQuestion ( qid, bid ) val) model.questions
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

targetToSubmission : Signal.Address Action -> (Maybe Int -> Action) -> String -> Signal.Message
targetToSubmission address f val =
  let
    mNumVal = val |> String.toInt |> Result.toMaybe
  in
    Signal.message address (f mNumVal)

viewQuestion : Signal.Address Action -> Question -> Html
viewQuestion address question =
  let
    (n1, n2, n3) = case question.nums of
                     x :: y :: z :: _ -> (x, y, z)
                     _ -> (0, 0, 0)
    (g1, g2) = case question.boxes of
           (_, box1) :: (_, box2) :: _ -> (box1.guess, box2.guess)
           _ -> (Nothing, Nothing)
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
                            , completionClass question 1 |> Html.Attributes.class
                            , Html.Events.on "change"
                                  Html.Events.targetValue
                                  (targetToSubmission address (\x -> Submission question.id 1 x))
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
                    []
                    [ makeMath "y = "
                    , Html.input
                            [ Html.Attributes.type' "text"
                            , completionClass question 2 |> Html.Attributes.class
                            , Html.Events.on "change"
                                  Html.Events.targetValue
                                  (targetToSubmission address (\y -> Submission question.id 2 y))
                            ]
                            [ ]
                    , Html.button
                            [ "btn btn-side btn-inverse " ++ completionClass question 2 |> Html.Attributes.class ]
                            [ Html.node "i"
                                    [ Html.Attributes.class (faClass question 2) ]
                                    [ ]
                            ]
                    ]
            ]
          , Html.div
            [ Html.Attributes.class "bars" ]
            [ barCollage { height = 10, width = 350 }
                           [ [ (n1, red),  (n2, green) ]
                           , [ (Maybe.withDefault 0 g1, blue) ]
                           , [ (Maybe.withDefault 0 g1, blue), (Maybe.withDefault 0 g2, brown) ]
                           , [ (n3, purple) ]
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
