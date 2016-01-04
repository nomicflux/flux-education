module NumberLine where

--import StartApp.Simple
import Graphics.Collage exposing (collage, Form, moveX, moveY, rect, filled, group)
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
import Dict exposing (Dict)
import Terms exposing (Term(..), VarName, Formula(..), Equation(..), System, Operation(..))

-- Model

type alias NumberLineInfo =
  { start : Float
  , size : Float
  , color  : Color
  , attributes : List Attribute
  }

type alias Dimensions =
  { height : Int
  , width : Int
  }

type alias AnimationState = Maybe {prevTime : Time, elapsedTime: Time}

type alias ColoredTerm =
                       { term : Term
                       , color : Color
                       , negated : Bool
                       }

type alias NLState =
  { values : List (List ColoredTerm)
  , collageSize : Dimensions
  , animationState : AnimationState
  }

mkConstant : Int -> Term
mkConstant c = Constant { value = c }

mkVar : VarName -> Term
mkVar name = Variable { name = name, value = Nothing, prevValue = Nothing }

defaultDims : Dimensions
defaultDims = { height = 10, width = 350 }

type alias ColorRecord =
                { colors : List Color
                , assigned : Dict VarName Color
                }

mkRec : List Color -> ColorRecord
mkRec cols = { colors = cols, assigned = Dict.empty }

getValue : ColoredTerm -> Int
getValue ct =
  case ct.term of
    Constant k -> if ct.negated then (-1)*k.value else k.value
    Variable v ->
      let
        base = withDefault 0 v.value
      in
        if ct.negated then (-1)*base else base

getPrevValue : ColoredTerm -> Int
getPrevValue ct =
  case ct.term of
    Constant k -> getValue ct
    Variable v ->
      let
        base = withDefault 0 v.prevValue
      in
        if ct.negated then (-1)*base else base

hasChanged : ColoredTerm -> Bool
hasChanged cterm =
  getValue cterm /= getPrevValue cterm

setValue : ColoredTerm -> Maybe Int -> ColoredTerm
setValue cterm newVal =
  case cterm.term of
    Constant _ -> cterm
    Variable v -> {cterm | term = Variable { v | value = newVal, prevValue = v.value}}

updatePrevValue : ColoredTerm -> ColoredTerm
updatePrevValue cterm =
  case cterm.term of
    Constant _ -> cterm
    Variable v -> {cterm | term = Variable { v | prevValue = v.value }}

lookupColor : Term -> ColorRecord -> (Maybe Color, ColorRecord)
lookupColor term colorRec =
  let
    (nextColor, rstColors) = case colorRec.colors of
                               [] -> (Nothing, [])
                               (x :: xs) -> (Just x, xs)
  in
    case term of
      Constant k -> case Dict.get (toString k) colorRec.assigned of
                       Nothing -> (nextColor
                                  , {colorRec | colors = rstColors
                                    , assigned = Dict.insert (toString k)
                                                 (withDefault Color.white nextColor)
                                                 colorRec.assigned})
                       Just col-> (Just col, colorRec)
      Variable x -> case Dict.get x.name colorRec.assigned of
                      Nothing -> (nextColor
                                 , {colorRec | colors = rstColors
                                   , assigned = Dict.insert x.name
                                                (withDefault Color.white nextColor)
                                                colorRec.assigned})
                      Just col-> (Just col, colorRec)

defaultColors : List Color
defaultColors = [ Color.red
                , Color.yellow
                , Color.orange
                , Color.green
                , Color.blue
                , Color.purple
                , Color.black ]

colorTerm : Term -> Bool-> ColorRecord -> (ColoredTerm, ColorRecord)
colorTerm term negated rec =
  let
    (mcol, newRec) = lookupColor term rec
  in
    ({color = withDefault Color.white mcol, term = term, negated = negated}
    , newRec)

colorFormula : Formula -> Bool -> ColorRecord -> (List ColoredTerm, ColorRecord)
colorFormula form negated rec =
  case form of
    SimpleT t ->
      let (ct, cr) = colorTerm t negated rec
      in ([ ct ], cr)
    TreeT form1 op form2 ->
      let
        (cts1, cr1) = colorFormula form1 False rec
        (cts2, cr2) = colorFormula form2 (op == Subtract) cr1
      in
        (cts1 ++ cts2, cr2)

colorEquation : ColorRecord -> Equation -> (List (List ColoredTerm), ColorRecord)
colorEquation rec eq =
  case eq of
    Input _ -> ([[]], rec)
    Equation e ->
      let
        (ctsl, crl) = colorFormula e.lhs False rec
        (ctsr, crr) = colorFormula e.rhs False crl
      in
        ([ctsl, ctsr], crr)

colorSystem : System -> List Color -> List (List ColoredTerm)
colorSystem sys colors =
  let
    coloring = colorEquation (mkRec colors)
    (res, _) = List.foldl (\eq (accLst, accRec) ->
                             let (l, newRec) = colorEquation accRec eq
                             in (l :: accLst, newRec))
                          ([], mkRec colors)
                          sys
    withSpaces = List.intersperse [[]] res
  in
    List.concat withSpaces

init : List Color -> System -> NLState
init colors sys =
  let
    revSys = List.reverse sys
    vals = colorSystem revSys (colors ++ defaultColors)
  in
    { values = vals
    , collageSize = defaultDims
    , animationState = Nothing
    }

-- Update

type NLAction = UpdateVariable VarName (Maybe Int)
             | Tick Time

moveBar : Float -> Float -> Animation
moveBar start stop =
  if start == stop
  then
    animation 0 |> from stop |> to stop |> ease identity
  else
    animation 0 |> from start |> to stop |> ease easeOutElastic

updateVar : (VarName, Maybe Int) -> ColoredTerm -> ColoredTerm
updateVar (name, mval) oldTerm =
  case oldTerm.term of
    Constant _ -> oldTerm
    Variable oldVar ->
          if oldVar.name == name
          then
            setValue oldTerm mval
          else
            updatePrevValue oldTerm

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
       UpdateVariable name mval ->
         let
           updatedVars = List.map (List.map
                                   (updateVar (name, mval))
                                  ) state.values
         in
           ( { state | values = updatedVars, animationState = Nothing }, Effects.tick Tick)

-- View

type Attribute = Negated

termToAttr : ColoredTerm -> List Attribute
termToAttr cterm =
  if cterm.negated
  then
    [ Negated ]
  else
    [ ]

numberToBar : Time -> ColoredTerm -> NumberLineInfo
numberToBar t cterm =
  case cterm.term of
    Constant _ ->
      { start = 0
      , size = toFloat (getValue cterm)
      , color = cterm.color
      , attributes = termToAttr cterm
      }
    Variable v ->
      { start = 0
      , size =
        if hasChanged cterm
        then
          animate t (moveBar (toFloat (getPrevValue cterm)) (toFloat (getValue cterm)))
        else
          toFloat (getValue cterm)
      , color = cterm.color
      , attributes = termToAttr cterm
      }

addBars : NumberLineInfo -> NumberLineInfo -> NumberLineInfo
addBars x y =
  { start = y.start + y.size + x.start
  , size = x.size
  , color = x.color
  , attributes = x.attributes
  }

scanBars : List NumberLineInfo -> List NumberLineInfo
scanBars bars = List.scanl addBars { start = 0, size = 0, color = black, attributes = [ ] } bars

numberBarToRect : Float -> Float -> NumberLineInfo -> Form
numberBarToRect scale height nl =
  let
    width = nl.size*scale
    neg = List.member Negated nl.attributes
    mainRect = rect width (height - (if neg then 4 else 0))
             |> filled nl.color
    negRect = rect width 2
              |> filled Color.charcoal
  in
    (if neg
    then
      group [ mainRect, negRect ]
    else
      mainRect) |> moveX (nl.start*scale + width/2)

makeFullBar : Float -> Dimensions -> Int -> List NumberLineInfo -> List Form
makeFullBar scale dims ypos bars =
  let
    positionBars = scanBars bars
    rectIt = numberBarToRect scale (toFloat dims.height)
  in
    List.map (rectIt
              >> moveY ((toFloat ((ypos - 1)*dims.height*(-1))) + (toFloat dims.height))
             ) positionBars

barCollage : NLState -> Element
barCollage {values, collageSize, animationState} =
  let
    timePassed = case animationState of
                   Nothing -> 0
                   Just x -> x.elapsedTime
    numRows = List.length values
    xMax =
      values
        |> List.map (List.map getValue)
        |> List.map (List.sum >> abs)
        |> List.maximum
        |> Maybe.withDefault 0
        |> \x -> x + 5
    scale = (toFloat collageSize.width) / ((toFloat xMax) * 2)
    setsOfBars = List.map (List.map (numberToBar timePassed)) values
  in
    collage collageSize.width ((numRows + 1)*collageSize.height)
              (List.concat
               (List.indexedMap (makeFullBar scale collageSize) setsOfBars))

view : Signal.Address NLAction -> NLState -> Html
view address state =
 Html.div
        [ Html.Attributes.class "bars" ]
        [ (barCollage state)
          |> Html.fromElement
        ]
