module NumberLine where

import Graphics.Collage exposing (collage, Form, moveX, moveY, rect, filled)
import Graphics.Element exposing (Element)
import Color exposing (Color, black)
import Easing exposing (..)

type alias NumberLineInfo =
  { start : Float
  , size : Float
  , color  : Color
  }

type alias Dimensions =
  { height : Int
  , width : Int
  }

numberToBar : Int -> Color -> NumberLineInfo
numberToBar i c = { start = 0
                  , size = toFloat i
                  , color = c
                  }

addBars : NumberLineInfo -> NumberLineInfo -> NumberLineInfo
addBars x y =
  { start = y.start + y.size + x.start
  , size = x.size
  , color = x.color }

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

barCollage : Dimensions -> List (List (Int, Color)) -> Element
barCollage dims setsOfVals =
  let
    numRows = List.length setsOfVals
    xMax =
      setsOfVals
        |> List.map (List.map fst)
        |> List.map List.sum
        |> List.maximum
        |> Maybe.withDefault 0
    scale = (toFloat dims.width) / ((toFloat xMax) * 2)
    setsOfBars = List.map (List.map (uncurry numberToBar)) setsOfVals
  in
    collage dims.width (numRows*dims.height*2)
              (List.concat
                     (List.indexedMap (makeFullBar scale dims) setsOfBars))
