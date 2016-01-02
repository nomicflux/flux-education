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
--import Debug

-- Model

type alias EQID = Int

type alias Lesson =
                 { equations : List (EQID, EQState)
                 , completed : Bool
                 }

init : List (List String) -> List Color -> VisualType -> Lesson
init eqStrs colors visType =
  { equations = List.indexedMap (\ i eq ->
                                   (i
                                   , Equation.init
                                             colors
                                             visType
                                             eq))
                                eqStrs
  , completed = False
  }

-- Update

type Action = RelayEquation EQID EQAction
            | SendCompletion

port signalCompletion : Signal Bool
port signalCompletion = completedMailbox.signal

completedMailbox : Signal.Mailbox Bool
completedMailbox = Signal.mailbox False

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
  -- let
    --_ = (action, model) |> Debug.log "Updating Lesson1"
  -- in
    case action of
      SendCompletion ->
        ( { model | completed = True }, Effects.none )
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
        in
          ({model | equations = newEqs}, Effects.batch (completionAction :: effs))

-- View

viewEquation : Signal.Address EQAction -> EQState -> Html
viewEquation address eq =
  Html.div
      [ Html.Attributes.class "full-equation" ]
      [ (Equation.view address eq) ]

view : Signal.Address Action -> Lesson -> Html
view address model =
  Html.div
      [ Html.Attributes.class "all-equations" ]
      (List.map (\ (id, eqstate) -> viewEquation (Signal.forwardTo address (RelayEquation id)) eqstate)
          model.equations)


-- App

noEffect : Effects Action
noEffect = Effects.none

app : List (List String) -> List Color -> VisualType -> StartApp.App Lesson
app eqStrs colors visType =
  StartApp.start { init = (init eqStrs colors visType, noEffect)
                 , update = update
                 , view = view
                 , inputs = []
                 }

startLesson : List (List String) -> List Color -> VisualType -> StartApp.App Lesson
startLesson = app


-- main : Signal Html
-- main = app.html
