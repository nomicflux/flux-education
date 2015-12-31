module Lesson1 where

--import Terms exposing (System)
import Equation exposing (..)
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import StartApp
import Signal
import Effects exposing (Effects, Never)
import Task
--import Debug

-- Model

type alias EQID = Int

type alias Model =
                 { equations : List (EQID, EQState) }

mkSystem1 : Int -> Int -> List String
mkSystem1 x y = [ toString x ++ " + "  ++ toString y ++ " = bob"
                ]

mkSystem2 : Int -> Int -> List String
mkSystem2 x y = [ toString x ++ " - "  ++ toString y ++ " = y"
                ]

mkSystem3 : Int -> Int -> Int -> Int -> List String
mkSystem3 x y z w = [ toString x ++ " - "  ++ toString y ++ " = q"
                    , "q + " ++ toString z ++ " = " ++ toString (x - y + z)
                    , "q + r = " ++ toString (y - x + z + w)
                    ]

equations : List (List String)
equations = [ mkSystem1 2 3
            , mkSystem1 2 40
            , mkSystem2 5 3
            , mkSystem3 1 2 3 4
            ]

colors : List Color
colors = [ Color.red, Color.green, Color.blue, Color.yellow, Color.brown, Color.black, Color.orange ]

init : Model
init = { equations = List.indexedMap (\ i eq -> (i,Equation.init colors NumberLine eq)) equations }

-- Update

type Action = RelayEquation EQID EQAction

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

update : Action -> Model -> (Model, Effects Action)
update action model =
  -- let
    --_ = (action, model) |> Debug.log "Updating Lesson1"
  -- in
    case action of
      RelayEquation i act ->
        let
          eqEffs = List.map (updateEquation i act) model.equations
          newEqs = List.map fst eqEffs
          effs   = List.map snd eqEffs
        in
          ({model | equations = newEqs}, Effects.batch effs)

-- View

viewEquation : Signal.Address EQAction -> EQState -> Html
viewEquation address eq =
  Html.div
      [ Html.Attributes.class "full-equation" ]
      [ (Equation.view address eq) ]

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div
      [ Html.Attributes.class "all-equations" ]
      (List.map (\ (id, eqstate) -> viewEquation (Signal.forwardTo address (RelayEquation id)) eqstate)
          model.equations)


-- App

noEffect : Effects Action
noEffect = Effects.none

app : StartApp.App Model
app = StartApp.start { init = (init, noEffect) 
                     , update = update
                     , view = view
                     , inputs = []
                     }

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

main : Signal Html
main = app.html
