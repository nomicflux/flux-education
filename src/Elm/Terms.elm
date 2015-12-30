module Terms where

import Maybe exposing (oneOf, withDefault)
import Combine exposing (Parser, skip, string, regex)
import Combine.Infix exposing (..)
import String exposing (toInt)

type alias VarName = String

type Term = Constant { value : Int }
          | Variable
            { name : String
            , value : Maybe Int
            , prevValue : Maybe Int
            }

type Operation = Add
               | Subtract
               | Multiply
               | Divide
               | NoOp

type Formula = SimpleT Term
             | TreeT Formula Operation Formula

type alias VarBox =
  { name : VarName
  , currentValue : Maybe Int
  }

type Equation = Equation
  { rhs : Formula
  , lhs : Formula
  }
              | Input
  { lhs : Formula
  , input : VarBox
  }

type alias System = List Equation

colorTerms : List Color -> System 

newVariable : VarName -> Term
newVariable name = Variable { name = name, value = Nothing, prevValue = Nothing }

newBox : VarName -> VarBox
newBox name = { name = name, currentValue = Nothing }

constantParser : Parser Term
constantParser = (String.toInt >> Result.toMaybe >> withDefault 0 >> (\k -> Constant {value=k})) <$>
                 (regex "[0-9]+")

variableParser : Parser Term
variableParser = newVariable <$> (regex "[a-zA-Z][a-zA-Z0-9_]*")

termParser : Parser Term
termParser = (constantParser <|> variableParser)

opParser : Parser Operation
opParser =
  let
    toOp c = case c of
               "+" -> Add
               "-" -> Subtract
               "*" -> Multiply
               "/" -> Divide
               _   -> NoOp
  in
    toOp <$> (regex "[\\+\\-\\*\\/]")

formulaParser : Parser Formula
formulaParser =
  let
    singleTerm = SimpleT <$> termParser
    spaces = string " " |> skip
    allTerms = TreeT <$>
               formulaParser <*>
               (spaces *> opParser <* spaces) <*>
               formulaParser
  in
    singleTerm <|> allTerms

inputParser : Parser Equation
inputParser = (\name -> Input { lhs = SimpleT ( newVariable name ), input = newBox name }) <$>
              regex "^\\?(.+)$"

plainEqParser : Parser Equation
plainEqParser =
  let
    spaces = string " " |> skip
    middle = spaces *> string "=" *> spaces
  in
    (\l r -> Equation {lhs = l, rhs = r}) <$>
                                          formulaParser <* middle <*>
                                          formulaParser


equationParser : Parser Equation
equationParser = inputParser <|> plainEqParser

takeString : String -> Maybe Equation
takeString s =
  case Combine.parse (equationParser <* Combine.end) s of
    (Combine.Done eq, _) -> Just eq
    _ -> Nothing

evalTerm : Term -> List (VarName, Maybe Int) -> Maybe Int
evalTerm term vals =
  let
    matchVar : Term -> (VarName, Maybe Int) -> Maybe Int
    matchVar t (vn, vx) =
      case t of
        Constant x -> Just x.value
        Variable x -> if vn == x.name then vx else Nothing
  in
    case term of
      Constant x -> Just x.value
      Variable x -> oneOf (List.map (matchVar term) vals)

evalFormula : List (VarName, Maybe Int) -> Formula -> Maybe Int
evalFormula vals formula =
  let
    thisEval = evalFormula vals
  in
    case formula of
      SimpleT t -> evalTerm t vals
      TreeT form1 op form2 ->
        case op of
          Add -> Maybe.map2 (+) (thisEval form1) (thisEval form2)
          Subtract -> Maybe.map2 (-) (thisEval form1) (thisEval form2)
          Multiply -> Maybe.map2 (*) (thisEval form1) (thisEval form2)
          Divide -> Maybe.map2 (//) (thisEval form1) (thisEval form2)
          NoOp -> Nothing

checkEquation : List (VarName, Maybe Int) -> Equation -> Maybe Bool
checkEquation vals eq =
  let
    (lhs, rhs) =
      case (eq) of
        Equation eq -> (evalFormula vals eq.lhs
                      , evalFormula vals eq.rhs)
        Input i -> (evalFormula vals i.lhs
                  , i.input.currentValue)
  in
    case (lhs, rhs) of
      (Just x, Just y) -> Just (x == y)
      _ -> Nothing

getInputVals : System -> List (VarName, Maybe Int)
getInputVals sys =
  case sys of
    [] -> []
    (Equation _ :: xs) -> getInputVals xs
    (Input i :: xs) -> (i.input.name, i.input.currentValue) :: getInputVals xs

checkBoxes : List (VarName, Maybe Int) -> System -> List (Maybe Bool, VarBox)
checkBoxes vals sys = case sys of
                   [] -> []
                   (Equation _ :: xs) -> checkBoxes vals xs
                   (i :: xs) ->
                     case i of
                       (Input ibox) -> (checkEquation vals i, ibox.input) :: checkBoxes vals xs
                       _ -> checkBoxes vals xs


checkSystem : System -> Maybe (List (Maybe Bool, VarBox))
checkSystem sys =
  let
    vals = getInputVals sys
    allChecked = List.map (checkEquation vals) sys
    anyBad = List.any (\mv -> mv == Just False) allChecked
    -- allGood = List.all (\mv -> mv == Just True) allChecked
  in
    if anyBad
    then
      Nothing
    else
      Just (checkBoxes vals sys)
