module Equation where

import Html exposing (Html)
import Html.Attributes
import NumberLine exposing (NLState, NLAction)
import Question exposing (QState, QAction)
import Terms exposing (..)
import Effects exposing (Effects)
import Color exposing (Color)
import Maybe exposing (withDefault)
import Signal
--import Debug

-- Model

type VisualType = NumberLine
                | NoVisual

type Visual = NLVisual NLState

type alias EQState =
  { system : System
  , question : QState
  , visual : Maybe Visual
  }

loadVisual : VisualType -> System -> List Color -> Maybe Visual
loadVisual vis sys colors =
  let
    loadFunc = case vis of
                 NumberLine -> Just << NLVisual << (NumberLine.init colors)
                 NoVisual -> always Nothing
  in
    loadFunc sys

cleanList : List (Maybe a) -> List a
cleanList ml =
  case ml of
    [] -> []
    (Nothing :: xs) -> cleanList xs
    (Just x :: xs) -> x :: cleanList xs

init : List Color -> VisualType -> List String -> EQState
init colors vis sysStr =
  let
    sys = cleanList (List.map Terms.stringToSystem sysStr)
  in
    { system = sys
    , question = Question.mkQuestion sys
    , visual = loadVisual vis sys colors
    }

-- Update

type EQAction = UpdateValue VarName (Maybe Int)
              | RelayQuestion QAction
              | RelayVisual NLAction

updateBox : VarBox -> (VarName, Maybe Int) -> VarBox
updateBox box (name, mval) =
  if name == box.name
  then
    { box | currentValue = mval }
  else
    box

updateBoxes : System -> (VarName, Maybe Int) -> System
updateBoxes sys val =
  case sys of
    [] -> sys
    (x :: xs) ->
      case x of
        Equation _ -> x :: updateBoxes xs val
        Input i ->
          Input { i | input = updateBox i.input val } :: updateBoxes xs val

sendQuestionUpdate : QState -> (VarName, Maybe Int) -> QState
sendQuestionUpdate q (name, value) = Question.update (Question.UpdateBox q.equations name value) q

sendNumberLineUpdate : NLState -> (VarName, Maybe Int) -> (NLState, Effects NLAction)
sendNumberLineUpdate nl (name, value) = NumberLine.update nl (NumberLine.UpdateVariable name value)

applyToFirst : (a -> b) -> (a,c) -> (b,c)
applyToFirst f (a,c) = (f a, c)

applyToSecond : (b -> d) -> (a,b)  -> (a,d)
applyToSecond f (a,b) = (a, f b)

applyToBoth : ((a -> c, b -> d)) -> (a,b) -> (c,d)
applyToBoth (f,g) (a,b) = (f a, g b)

update : EQState -> EQAction -> (EQState, Effects EQAction)
update state action =
  -- let
    --_ = (state, action) |> Debug.log "Updating Equations Redux"
  -- in
    case action of
      UpdateValue name mval ->
        let
          newSys = updateBoxes state.system (name, mval)
          newQ   = sendQuestionUpdate state.question (name, mval)
          (newVis, nlEff) = case state.visual of
                              Nothing        -> (state.visual, Effects.none)
                              Just (NLVisual nlState) -> applyToFirst
                                                         (Just << NLVisual)
                                                         (NumberLine.update nlState
                                                          (NumberLine.UpdateVariable name mval))
        in
          ( { state | system = newSys, question = newQ, visual = newVis }
          , Effects.map RelayVisual nlEff
          )
      RelayVisual visAction ->
          case state.visual of
            Nothing -> (state, Effects.none)
            Just (NLVisual nlState) ->
              let
                (newVis, eff) = applyToBoth (Just << NLVisual, Effects.map RelayVisual)
                                (NumberLine.update nlState visAction)
              in
                ({state | visual = newVis}, eff)
      RelayQuestion qAction ->
        case qAction of
          Question.UpdateBox sys name mval ->
            let
              newSys = updateBoxes state.system (name, mval)
              newQ = Question.update qAction state.question
              (newVis, nlEff) = case state.visual of
                                  Nothing        -> (state.visual, Effects.none)
                                  Just (NLVisual nlState) -> applyToFirst
                                                             (Just << NLVisual)
                                                             (NumberLine.update nlState
                                                              (NumberLine.UpdateVariable name mval))
              --_ = Debug.log "NL Effects" nlEff
            in
              ( { state | system = newSys, question = newQ, visual = newVis }
              , Effects.map RelayVisual nlEff
              )
          _ ->
            let
              newQ = Question.update qAction state.question
            in
              ({state | question = newQ}, Effects.none)

-- View

view : Signal.Address EQAction -> EQState -> Html
view address state =
  let
    visual = case state.visual of
               Nothing -> Html.div [ ] [ ]
               Just (NLVisual nlState) ->
                 NumberLine.view (Signal.forwardTo address RelayVisual) nlState
  in
    Html.div
          [ Html.Attributes.class "question" ]
          [ (Question.view (Signal.forwardTo address RelayQuestion) state.question)
          ,  visual ]
