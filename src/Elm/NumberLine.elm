module NumberLine where

--import StartApp.Simple
import Graphics.Collage exposing (collage, Form, moveX, moveY, rect, filled)
import Graphics.Element exposing (Element)
import Color exposing (Color, black)
import Easing exposing (Easing, easeOutElastic)
import Animation exposing (..)
import Time exposing (second, Time)
import Effects exposing (tick, Effects)
import Signal
import Maybe exposing (withDefault)
import Html exposing (Html)
import Html.Attributes
import Terms exposing (Term(..), VarName)

-- Model

type alias NumberLineInfo =
  { start : Float
  , size : Float
  , color  : Color
  }

type alias Dimensions =
  { height : Int
  , width : Int
  }

type alias AnimationState = Maybe {prevTime : Time, elapsedTime: Time}

type alias NLState =
  { values : List (List (Term, Color))
  , collageSize : Dimensions
  , animationState : AnimationState
  }

mkConstant : Int -> Term
mkConstant c = Constant { value = c }

mkVar : VarName -> Term
mkVar name = Variable { name = name, value = Nothing, prevValue = Nothing }

defaultDims : Dimensions
defaultDims = { height = 10, width = 350 }

-- init : Dimensions -> List (List (Term, Color)) -> NLState
-- init dims nums =
--   { values = nums
--   , collageSize = dims
--   , animationState = Nothing
--   }

defaultColors : List Color
defaultColors = [ red, yellow, orange, green, blue, purple, black ]

init :  

-- Update

type NLAction = UpdateVariable VarName Int
             | Tick Time

moveBar : Float -> Float -> Animation
moveBar start stop =
  if start == stop
  then
    animation 0 |> from stop |> to stop |> ease identity
  else
    animation 0 |> from start |> to stop |> ease easeOutElastic

updateVar : Term -> Term -> Term
updateVar oldTerm newTerm =
  case oldTerm of
    Constant {value} -> oldTerm
    Variable oldVal ->
      case newTerm of
        Constant x -> oldTerm
        Variable newVal ->
          if oldVal.name == newVal.name
          then
            Variable {name=oldVal.name, value=newVal.value, prevValue=oldVal.value}
          else
            Variable {name=oldVal.name, value=oldVal.value, prevValue=oldVal.value}

duration : Time
duration = 1 * second

update : NLState -> NLAction -> (NLState, Effects NLAction)
update state action =
  case action of
    Tick clockTime ->
      let
        newElapsedTime =
          case state.animationState of
            Nothing -> 0
            Just {elapsedTime, prevTime} ->
              elapsedTime + (clockTime - prevTime)
      in
        if newElapsedTime > duration then
          ( { state | animationState = Just { elapsedTime = duration, prevTime = clockTime }}, Effects.none )
        else
          ( { state | animationState = Just { elapsedTime = newElapsedTime, prevTime = clockTime } }
          , Effects.tick Tick
          )
    UpdateVariable name val ->
      let
        updatedVars = List.map (List.map
                                      (\ (t,c) ->
                                         (updateVar t (Variable {name=name, value=Just val, prevValue=Nothing}), c)
                                      )) state.values
      in
        ( { state | values = updatedVars, animationState = Nothing }, Effects.tick Tick)

-- View

numberToBar : Time -> Term -> Color -> NumberLineInfo
numberToBar t term c =
  case term of
    Constant {value} ->
      { start = 0
      , size = toFloat value
      , color = c
      }
    Variable {name, value, prevValue} ->
      { start = 0
      , size = animate t (moveBar (toFloat (withDefault 0 prevValue)) (toFloat (withDefault 0 value)))
      , color = c
      }

addBars : NumberLineInfo -> NumberLineInfo -> NumberLineInfo
addBars x y =
  { start = y.start + y.size + x.start
  , size = x.size
  , color = x.color
  }

scanBars : List NumberLineInfo -> List NumberLineInfo
scanBars bars = List.scanl addBars { start = 0, size = 0, color = black } bars

numberBarToRect : Float -> Float -> NumberLineInfo -> Form
numberBarToRect scale height nl =
  let
    width = nl.size*scale
  in
    rect width height
      |> filled nl.color
      |> moveX (nl.start*scale + width/2)

makeFullBar : Float -> Dimensions -> Int -> List NumberLineInfo -> List Form
makeFullBar scale dims ypos bars =
  let
    positionBars = scanBars bars
    rectIt = numberBarToRect scale (toFloat dims.height)
  in
    List.map (rectIt
              >> moveY ((toFloat ((ypos - 1)*dims.height*(-1))) + (toFloat dims.height) / 2)
             ) positionBars

barCollage : Dimensions -> List (List (Term, Color)) -> AnimationState -> Element
barCollage dims setsOfVals anistate =
  let
    timePassed = case anistate of
                   Nothing -> 0
                   Just x -> x.elapsedTime
    numRows = List.length setsOfVals
    xMax =
      setsOfVals
        |> List.map (List.map (termToInt << fst))
        |> List.map (List.sum >> abs)
        |> List.maximum
        |> Maybe.withDefault 0
        |> \x -> x + 5
    scale = (toFloat dims.width) / ((toFloat xMax) * 2)
    setsOfBars = List.map (List.map (uncurry (numberToBar timePassed))) setsOfVals
  in
    collage dims.width (numRows*dims.height*2)
              (List.concat
               (List.indexedMap (makeFullBar scale dims) setsOfBars))

termToInt : Term -> Int
termToInt term =
  case term of
    Constant c -> c.value
    Variable v -> withDefault 0 v.value

view : Signal.Address NLAction -> NLState -> Html
view address state =
 Html.div
        [ Html.Attributes.class "bars" ]
        [ (barCollage state.collageSize
                      state.values
                      state.animationState)
--                      (List.map (List.map (\ (t, c) -> (termToInt t, c))) state.values))
          |> Html.fromElement
        ]

-- App

-- main = StartApp.Simple.start
--        { model = { values = []
--                  , dimensions = { height = 0, width = 0 }
--                  }
--        , update = update
--        , view = view
--        }
