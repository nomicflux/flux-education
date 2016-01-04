module Question where

import List
import Html exposing (Html)
import Html.Attributes
import Html.Events
import String
import Terms exposing (Equation(..), System, VarName, VarBox, Operation(..), Term(..), Formula(..), checkSystem)
import Maybe exposing (withDefault)
-- import Debug

-- Model

type alias QuestionID = Int

type alias BoxID = VarName

type alias InputBox =
  { completed : Maybe Bool
  , attempted : Bool
  , guess : Maybe Int
  , id : BoxID
  }

type alias QState =
  { boxes : List InputBox
  --, equations : System
  --, id : QuestionID
  }

mkBox : VarBox -> InputBox
mkBox box = { completed = Nothing
            , attempted = False
            , guess = Nothing
            , id = box.name
            }

mkQuestion : System -> QState
mkQuestion sys =
  let
    boxes = List.map snd (withDefault [] (checkSystem sys))
  in
    { boxes = List.map mkBox boxes
    --, equations = sys
    --, id = qid + 1
    }

questionCompleted : QState -> Bool
questionCompleted state = List.all (\ b -> b.completed == Just True) state.boxes

-- Update

type QAction = UpdateBox System BoxID (Maybe Int)
             | Submission VarName (Maybe Int)

swapUpdateSystem : QAction -> System -> QAction
swapUpdateSystem action newSys =
  case action of
    UpdateBox oldSys box mval -> UpdateBox newSys box mval
    _ -> action

update : QAction -> QState -> QState
update action state =
  case action of
    UpdateBox sys bid val ->
      let
        newBoxes = List.map (updateBox sys bid val) state.boxes
      in
        { state | boxes = newBoxes }
    Submission name mval -> state

validateBox : System -> InputBox -> Maybe Int -> Maybe Bool
validateBox sys box val = Terms.checkVariable sys box.id

updateBox : System -> BoxID -> Maybe Int -> InputBox -> InputBox
updateBox sys wantedBid mval box =
  if box.id == wantedBid
  then
    { box | attempted = True, completed = validateBox sys box mval, guess = mval }
  else
    { box | completed = validateBox sys box mval }

-- View

completionClass : InputBox -> String
completionClass box =
  if box.completed == Just True
  then
    "completed"
  else
    if box.attempted && box.completed == Just False
    then
      "incorrect"
    else
      "new-question"

faClass : InputBox -> String
faClass box =
  if box.completed == Just True
  then
    "fa fa-check-square-o"
  else
    if box.attempted && box.completed == Just False
    then
      "fa fa-square-o"
    else
      "fa fa-square-o"

boxesComplete : QState -> Bool
boxesComplete state = List.all (\ b -> b.completed == Just True ) state.boxes

-- findLatestAnswered : List QState -> QuestionID
-- findLatestAnswered qs = qs
--                       |> List.filter boxesComplete
--                       |> List.map (.id)
--                       |> List.maximum
--                       |> Maybe.withDefault 0

makeMath : String -> Html
makeMath s = Html.math [] [ Html.text s ]

termToHtml : Term -> Html
termToHtml term =
  case term of
    Constant { value } ->
      Html.span
        [ Html.Attributes.class "term term-constant" ]
        [ value |> toString |> Html.text ]
    Variable { name } ->
      Html.span
          [ Html.Attributes.class "term term-variable" ]
          [ Html.text name ]

opToHtml : Operation -> Html
opToHtml op =
  let
    t = case op of
          Add -> "+"
          Subtract -> "-"
          Multiply -> "*"
          Divide -> "/"
          NoOp -> "?"
  in
    Html.span
        [ Html.Attributes.class "operation" ]
        [ Html.text t ]

formulaToHtml : Formula -> Html
formulaToHtml form =
  case form of
    SimpleT term ->
      Html.span
          [ Html.Attributes.class "formula formula-simple" ]
          [ termToHtml term ]
    TreeT form1 op form2 ->
      Html.span
          [ Html.Attributes.class "formula formula-tree" ]
          [ formulaToHtml form1
          , opToHtml op
          , formulaToHtml form2
          ]

targetToSubmission : Signal.Address QAction -> (Maybe Int -> QAction) -> String -> Signal.Message
targetToSubmission address f val =
  let
    mNumVal = val |> String.toInt |> Result.toMaybe
  in
    Signal.message address (f mNumVal)

findBox : QState -> VarBox -> Maybe InputBox
findBox state vbox =
  List.head (List.filter (\b -> b.id == vbox.name) state.boxes)

boxToHtml : Signal.Address QAction -> QState -> System -> VarBox -> Html
boxToHtml address state sys vbox =
  let
    mbox = findBox state vbox
    boxHtml = case mbox of
                Nothing -> [ ]
                Just box ->
                  [ Html.input
                    [ Html.Attributes.type' "text"
                    , completionClass box |> Html.Attributes.class
                    , Html.Events.on "change"
                          Html.Events.targetValue
                          (targetToSubmission address (\x -> UpdateBox sys box.id x))
                    ]
                    [ ]
                  , Html.button
                          [ "btn btn-side " ++ completionClass box |> Html.Attributes.class ]
                          [ Html.node "i"
                                  [ Html.Attributes.class (faClass box) ]
                                  [ ]
                          ]
                  ]
  in
    Html.span
          [ Html.Attributes.class "formula formula-input" ]
          boxHtml

eqToHtml : Signal.Address QAction -> QState -> System -> Equation -> Html
eqToHtml address state sys eq =
  case eq of
    Equation e ->
      Html.div
          [ Html.Attributes.class "equation equation-plain" ]
          [ formulaToHtml e.lhs
          , Html.text " = "
          , formulaToHtml e.rhs
          ]
    Input i ->
      Html.div
          [ Html.Attributes.class "equation equation-input" ]
          [ formulaToHtml i.lhs
          , Html.text " = "
          , boxToHtml address state sys i.input
          ]

view : Signal.Address QAction -> QState -> System -> Html
view address state sys =
  let
    fullEqToHtml = eqToHtml address state sys
  in
    Html.div
          [ Html.Attributes.class "blanks" ]
          (List.map fullEqToHtml sys)
