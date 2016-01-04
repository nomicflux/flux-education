module Lesson where

--import Terms exposing (System)
import Equation exposing (..)
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import StartApp
import Signal
import Effects exposing (Effects, Never)
import Task exposing (Task)
-- import Http

-- Model

type alias EQID = Int

type alias Lesson =
                 { equations : List (EQID, EQState)
                 , numberShowing : Int
                 , completed : Bool
                 }

blankInit : Lesson
blankInit =
  { equations = [ ]
  , numberShowing = 1
  , completed = False
  }

fullInit : List (List String) -> Maybe (List Color) -> VisualType -> Lesson
fullInit eqStrs mcolors visType =
  let
    eqs = List.indexedMap (\ i eq ->
                             (i + 1
                             , Equation.init
                                       mcolors
                                       visType
                                       eq))
          eqStrs
  in
    { equations = eqs
    , numberShowing = 1
    , completed = False
    }

equationsToShow : Lesson -> Int
equationsToShow state =
    state.equations
      |> List.filter (\ eq -> not (Equation.equationCompleted (snd eq)))
      |> List.map fst
      |> List.minimum
      |> Maybe.withDefault (List.length state.equations)

getCurrentPos : Lesson -> Int
getCurrentPos model =
  List.map fst model.equations
      |> List.maximum
      |> Maybe.withDefault 0

-- Update

type Action = RelayEquation EQID EQAction
            | LoadEquations (List (List String)) (Maybe (List Color)) VisualType
            | AddEquation (List String) (Maybe (List Color)) VisualType
            | SendCompletion
            | NoOp

port signalCompletion : Signal Bool
port signalCompletion = completedMailbox.signal

completedMailbox : Signal.Mailbox Bool
completedMailbox = Signal.mailbox False

-- getEquations : Lesson -> Task Never Action
-- getEquations lesson =
--   Http.get lessonDecoder ("/lessons/equations/" ++ toString lesson.id )
--     |> Task.toMaybe
--     |> Task.map LoadEquations

updateEquation : EQID -> EQAction -> (EQID, EQState) -> ((EQID, EQState), Effects Action)
updateEquation wantedId action (currId, eqstate) =
  if wantedId == currId
  then
    let
      (newState, eqeff) = Equation.update eqstate action
    in
      ((currId, newState), Effects.map (RelayEquation currId) eqeff)
  else
    ((currId, eqstate), Effects.none)

update : Action -> Lesson -> (Lesson, Effects Action)
update action model =
    case action of
      SendCompletion ->
        ( { model | completed = True, numberShowing = List.length model.equations }, Effects.none )
      LoadEquations eqs mcolors visType ->
        ( fullInit eqs mcolors visType, Effects.none )
      AddEquation eq mcolors visType ->
        let
          currPos = (getCurrentPos model)
          newEq = (currPos + 1, Equation.init mcolors visType eq)
        in
          ( { model | equations = model.equations ++ [newEq]}, Effects.none )
      RelayEquation i act ->
        let
          eqEffs = List.map (updateEquation i act) model.equations
          newEqs = List.map fst eqEffs
          effs   = List.map snd eqEffs
          completionAction =
            if List.all (\ eq -> Equation.equationCompleted (snd eq)) model.equations
            then
              Signal.send completedMailbox.address True
                  |> Task.map (always SendCompletion)
                  |> Effects.task
            else
              Effects.none
          numberShowing = equationsToShow model
        in
          ({model | equations = newEqs, numberShowing = numberShowing}
          , Effects.batch (effs ++ [completionAction] ))
      NoOp -> ( model, Effects.none )

-- View

viewEquation : Signal.Address EQAction -> EQState -> Html
viewEquation address eq =
  Html.div
      [ Html.Attributes.class "full-equation" ]
      [ (Equation.view address eq) ]

view : Signal.Address Action -> Lesson -> Html
view address model =
  let
    eqsToShow = List.take (model.numberShowing) model.equations
  in
  Html.div
      [ Html.Attributes.class "all-equations" ]
      (List.map (\ (id, eqstate) -> viewEquation (Signal.forwardTo address (RelayEquation id)) eqstate)
          eqsToShow)

-- App

type alias JsonColor = (Float, Float, Float, Float)

jsonToColor : JsonColor -> Color
jsonToColor (h, s, l, a) = Color.hsla (degrees h) s l a

type alias InputData =
                     { equations : List (List String)
                     , mcolors : Maybe (List JsonColor)
                     , visual : String
                     }

port sendLesson : Signal (Maybe InputData)

transformData : Maybe InputData -> Action
transformData minputs =
    case minputs of
      Just ins ->
        LoadEquations ins.equations
                      (Maybe.map (List.map jsonToColor) ins.mcolors)
                      (Equation.stringToVisualType ins.visual)
      Nothing -> NoOp

app : StartApp.App Lesson
app =
  StartApp.start { init = (blankInit, Effects.none)
                 , update = update
                 , view = view
                 , inputs = [ Signal.map transformData sendLesson ]
                 }

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
