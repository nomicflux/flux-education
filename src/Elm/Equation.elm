module Equation where

import Html exposing (Html)
import Html.Attributes
import NumberLine exposing (NLState, NLAction)
import Question exposing (QState, QAction, QuestionID)
import Terms exposing (..)
import Effects exposing (Effects)
import Color exposing (Color)
import Signal

-- Model

type Visual = NLVisual (Maybe NLState)

type alias EQState =
  { system : System
  , question : QState
  , visual : Visual
  }

loadVisual : Visual -> System -> List Color -> Visual
loadVisual vis sys colors =
  let
    loadFunc = case vis of
                 NLVisual _ -> NLVisual << Just << (NumberLine.init colors)
  in
    loadFunc sys

--mkQBatch : List System -> List (QuestionID, QState)
--mkQBatch = List.indexedMap (\(i,s) -> (i, mkQuestion i s))

init : System -> Visual -> EQState
init sys vis =
    { system = sys
    , question = Question.mkQuestion sys
    , visual = loadVisual vis sys
    }

-- Update

type EQAction = UpdateValue VarName (Maybe Int)
              | RelayQuestion QAction
              | RelayVisual NLAction

updateBox : VarBox -> (VarName, Maybe Int) -> (VarBox, Effects EQAction)
updateBox box (name, mval) =
  if name == box.name
  then
    { box | currentValue = mval }
  else
    (box, Effects.none)

updateBoxes : System -> (VarName, Maybe Int) -> System
updateBoxes sys val =
  case sys of
    [] -> sys
    (x :: xs) ->
      case x of
        Equation _ -> x :: updateBoxes xs
        Input i ->
          { x | input = updateBox i.input val } :: updateBoxes xs

sendQuestionUpdate : QState -> (VarName, Maybe Int) -> QState
sendQuestionUpdate q (name, value) = Question.update q (Question.UpdateBox q.system name value)

sendNumberLineUpdate : NLState -> (VarName, Maybe Int) -> (NLState, Effects NLAction)
sendNumberLineUpdate nl (name, value) = NumberLine.update nl (NumberLine.UpdateVariable name value)

update : EQState -> EQAction -> (EQState, Effects EQAction)
update state action =
  case action of
    UpdateValue name val ->
      let
        newSys = updateBoxes state.system (name, val)
        newQ   = sendQuestionUpdate (state.question (name, val))
        (newVis, eff) = case state.visual of
                          NLVisual Nothing        -> (state.visual, Effects.none)
                          NLVisual (Just nlState) -> (NumberLine.update nlState (name, val))
      in
        ( { state | system = newSys, question = newQ, visual = newVis }
        , eff
        )
    RelayVisual visAction ->
      case state.visual of
        NLVisual Nothing -> (state, Effects.none)
        NLVisual (Just nlState) -> (NumberLine.update nlState visAction)
    _ -> (state, Effects.none)

-- View

view : Signal.Address -> EQState -> Html
view address state =
  let
    visual = case state.visual of
               NLVisual Nothing -> Html.div [ ] [ ]
               NLVisual (Just nlState) ->
                 NumberLine.view (Signal.forwardTo address RelayVisual) nlState
  in
    Html.div
          [ Html.Attributes.class "question" ]
          [ (Question.view (Signal.forwardTo address RelayQuestion) state.question)
          ,  visual ]
