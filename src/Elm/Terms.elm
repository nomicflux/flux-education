module Terms where

import Maybe exposing (oneOf, withDefault)
import Combine exposing (Parser(..), skip, string, regex)
import Combine.Infix exposing (..)
import String exposing (toInt)
import Set exposing (Set)
import Debug

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

newVariable : VarName -> Term
newVariable name = Variable { name = name, value = Nothing, prevValue = Nothing }

newBox : VarName -> VarBox
newBox name = { name = name, currentValue = Nothing }

constantParser : Parser Term
constantParser = ((String.toInt >> Result.toMaybe >> withDefault 0 >> (\k -> Constant {value=k})) <$>
                 (regex "[0-9]+"))

variableParser : Parser Term
variableParser = (newVariable <$> (regex "[a-zA-Z][a-zA-Z0-9_]*"))

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
    (toOp <$> (regex "[\\+\\-\\*\\/]"))

formulaParser : Parser Formula
formulaParser =
  let
    singleTerm = (SimpleT <$> termParser)
    spaces = Combine.many (string " ")
    allTerms = Combine.rec (\() -> TreeT <$>
                     singleTerm <*>
                     (spaces *> opParser <* spaces) <*>
                     singleTerm)
  in
    Combine.rec (\() -> allTerms <|> singleTerm)

inputParser : Parser Equation
inputParser = ((\name -> Input { lhs = SimpleT ( newVariable name ), input = newBox name }) <$>
              (string "?" *> regex "(.+)$"))

plainEqParser : Parser Equation
plainEqParser =
  let
    spaces = Combine.many (string " ")
    middle = spaces *> string "=" *> spaces
  in
    ((\l r -> Equation {lhs = l, rhs = r}) <$>
                                          (formulaParser) <*>
                                          (middle *> formulaParser))

equationParser : Parser Equation
equationParser = (plainEqParser <|> inputParser)

stringToEquation : String -> Maybe Equation
stringToEquation s =
  case Combine.parse (equationParser) s of
    (Combine.Done eq, _) -> Just eq
    (Combine.Fail f, x) -> Debug.log "Failure" (f,x) |> always Nothing

cleanList : List (Maybe a) -> List a
cleanList ml =
  case ml of
    [] -> []
    (Nothing :: xs) -> cleanList xs
    (Just x :: xs) -> x :: cleanList xs

genSystem : List String -> System
genSystem strs =
  let
    eqs = List.map stringToEquation strs |> cleanList |> List.reverse
    vars = List.foldl (\eq acc -> eqGetVars eq |> Set.union acc) Set.empty eqs |> Set.toList
    mkInput n = stringToEquation ("?" ++ n)
  in
    eqs ++ (List.map mkInput vars |> cleanList)

nullEq : Equation
nullEq = Equation { lhs = SimpleT (Constant {value = 0})
                  , rhs = SimpleT (Constant {value = 0})}

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
    _ = (lhs, rhs, vals)
  in
    case (lhs, rhs) of
      (Just x, Just y) -> Just (x == y)
      _ -> Nothing

formGetVars : Formula-> Set VarName
formGetVars form =
  case form of
    SimpleT (Constant _) -> Set.empty
    SimpleT (Variable v) -> Set.singleton v.name
    TreeT form1 _ form2 -> Set.union (formGetVars form1) (formGetVars form2)

eqGetVars : Equation -> Set VarName
eqGetVars eq =
  case eq of
    Input _ -> Set.empty
    Equation e -> Set.union (formGetVars e.lhs) (formGetVars e.rhs)

eqContainsVar : VarName -> Equation -> Bool
eqContainsVar name eq =
  Set.member name (eqGetVars eq)

checkVariable : System -> VarName -> Maybe Bool
checkVariable sys name =
  let
    vals = getInputVals sys
    getVar = List.filter (\(n,_) -> n == name) (vals)
    checkedEqs = List.map (checkEquation vals) (List.filter (eqContainsVar name) sys)
  in
    if List.length getVar /= 1
    then
      Nothing
    else
      List.foldl (Maybe.map2 (\a b -> a && b)) (Just True) checkedEqs

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
