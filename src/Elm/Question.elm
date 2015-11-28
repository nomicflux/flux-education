module Question where

import String exposing (String)

-- Model

type alias ID = Int

type Component = Definite Int
               | Indefinite String
               | Unknown

type Term = Singleton Component
          | Sum Term Term
          | Difference Term Term
          | Product Term Term
          | Division Term Term

type Equation = Equation Term Term

type alias Metaquation = { equation : Equation
                         , attempted : Bool
                         , completed : Bool
                         , id : ID
                         }
              
type alias Model =
  { equations : List Metaquation
  }

twoMaybes : Maybe a -> Maybe (List a) -> Maybe (List a)
twoMaybes mx my =
  case (mx, my) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just a, Just l) -> Just (a :: l)
                 
listToMaybe : List (Maybe a) -> Maybe (List a)
listToMaybe l = List.foldl twoMaybes (Just []) l

stringToComponent : String -> Maybe Component
stringToComponent str =
  let
    strNum = String.toInt str |> Result.toMaybe |> Maybe.map Definite
    ( h, r ) = uncons str |> Maybe.withDefault (' ', "")
    strVar = if Char.isLower h then Just (String.fromChar str) else Nothing |> Maybe.map Indefinite
    strPlace = if h == '?' then Just Unknown else Nothing
  in
    Maybe.oneOf [strNum, strVar, strPlace]
                
stringToEquation : String -> Maybe Equation
stringToEquation str =
  let
    parts = String.split "=" str
  in
    case parts of
      ( l, r ) ->
        let
          splitToComp = Strings.split "+"
                       >> List.map (stringToComponent << Strings.trim)
                       >> listToMaybe
          left = splitToInt l
          right = splitToInt r
        in
          case (left, right) of
            (Nothing, _) -> Nothing
            (_, Nothing) -> Nothing
            (Just ml, Just mr) -> Just (Equation ml mr)
      _ -> Nothing
                 
init : String -> Model
init str =
  let
    eqs = Strings.split ";" str
        |> List.map stringToEquation
        |> listToMaybe |> Maybe.withDefault []
  in
    List.indexedMap
          (\ (i, e) -> { equation = e
                       , attempted = False
                       , completed = False
                       , id = i
                       }
          ) eqs

-- Update

type Action = Submission ID Int

-- View
