module Equation where

import Html exposing (Html)
import Html.Attributes
import NumberLine exposing (NLState, NLAction)
import Question exposing (QState, QAction)
import Terms exposing (..)
import Effects exposing (Effects)
import Color exposing (Color)
import Maybe exposing (withDefault)
import String exposing (toLower)
import Signal

-- Model

type VisualType = NumberLine
                | NoVisual

stringToVisualType : String -> VisualType
stringToVisualType str =
  case toLower str of
    "numberline" -> NumberLine
    _ -> NoVisual

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

defaultColors : Int -> List Color
defaultColors n = [0..n]
                |> List.map (\ x -> (toFloat x) / (toFloat n) * 360)
                |> List.map (\ h -> Color.hsla (degrees h) 0.7 0.5 1)

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f lsta lstb =
  case (lsta, lstb) of
    ([], _) -> []
    (_, []) -> []
    ((x::xs), (y::ys)) -> f x y :: zipWith f xs ys

separateList : List a -> (List a, List a)
separateList lst =
  case lst of
    [] -> ([], [])
    (x::[]) -> ([x], [])
    (x::y::rst) ->
      let
        (l,r) = separateList rst
      in
        (x::l, y::r)

interweave : List a -> List a
interweave lst =
  let
    (lst1, lst2) = separateList lst
  in
    lst1 ++ lst2

colorsForSystem : System -> List Color
colorsForSystem sys =
  Terms.systemDistinctParts sys |> defaultColors |> interweave

init : Maybe (List Color) -> VisualType -> List String -> EQState
init mcolors vis sysStr =
  let
    sys = genSystem sysStr -- (List.map Terms.stringToSystem sysStr)
    colors = (Maybe.withDefault (colorsForSystem sys) mcolors)
  in
    { system = sys
    , question = Question.mkQuestion sys
    , visual = loadVisual vis sys colors
    }

equationCompleted : EQState -> Bool
equationCompleted state = Question.questionCompleted state.question

-- Update

type EQAction = RelayQuestion QAction
              | RelayVisual NLAction
              --| UpdateValue VarName (Maybe Int)

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

-- sendQuestionUpdate : QState -> (VarName, Maybe Int) -> QState
-- sendQuestionUpdate q (name, value) = Question.update (Question.UpdateBox q.equations name value) q

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
    case action of
      -- UpdateValue name mval ->
      --   let
      --     newSys = updateBoxes state.system (name, mval)
      --     newQ   = --sendQuestionUpdate state.question (name, mval)
      --     (newVis, nlEff) = case state.visual of
      --                         Nothing        -> (state.visual, Effects.none)
      --                         Just (NLVisual nlState) -> applyToFirst
      --                                                    (Just << NLVisual)
      --                                                    (NumberLine.update nlState
      --                                                     (NumberLine.UpdateVariable name mval))
      --   in
      --     ( { state | system = newSys, question = newQ, visual = newVis }
      --     , Effects.map RelayVisual nlEff
      --     )
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
              newQ = Question.update (Question.swapUpdateSystem qAction newSys) state.question
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
          [ (Question.view (Signal.forwardTo address RelayQuestion) state.question state.system)
          ,  visual ]
