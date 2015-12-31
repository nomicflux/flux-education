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
  { completed : Bool
  , attempted : Bool
  , guess : Maybe Int
  , id : BoxID
  }

type alias QState =
  { boxes : List InputBox
  , equations : System
  --, id : QuestionID
  }

mkBox : VarBox -> InputBox
mkBox box = { completed = False
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
    , equations = sys
    --, id = qid + 1
    }

-- Update

type QAction = UpdateBox System BoxID (Maybe Int)
             | Submission VarName (Maybe Int)

update : QAction -> QState -> QState
update action state =
  let
    fullUpdateBox = updateBox state
    -- _ = (state, action) |> Debug.log "Updating Questions"
  in
  case action of
    UpdateBox sys bid val ->
      let
        newBoxes = List.map (fullUpdateBox bid val) state.boxes
      in
        { state | equations = sys, boxes = newBoxes }
    Submission name mval -> state

validateBox : QState -> InputBox -> Maybe Int -> Bool
validateBox state box val = False-- checkSystem state.equations
                            -- Implement!

updateBox : QState -> BoxID -> Maybe Int -> InputBox -> InputBox
updateBox state wantedBid mval box =
  if box.id == wantedBid
  then
    { box | attempted = True, completed = validateBox state box mval, guess = mval }
  else
    box

-- updateQuestion : ( QuestionID, BoxID ) -> Int -> QState -> QState
-- updateQuestion (qid, bid) val question =
--   if question.id == qid
--   then
--     let
--       eqs = Terms.updateBoxes (bid, val)
--       boxes = List.map (updateBox bid val) question.boxes
--     in
--       { question | equations = eqs, boxes = boxes }
--   else
--     question

-- View

completionClass : InputBox -> String
completionClass box =
  if box.completed
  then
    "completed"
  else
    if box.attempted
    then
      "incorrect"
    else
      "new-question"

faClass : InputBox -> String
faClass box =
  if box.completed
  then
    "fa fa-check-square-o"
  else
    if box.attempted
    then
      "fa fa-square-o"
    else
      "fa fa-square-o"

boxesComplete : QState -> Bool
boxesComplete state = List.all (\ b -> b.completed ) state.boxes

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

boxToHtml : Signal.Address QAction -> QState -> VarBox -> Html
boxToHtml address state vbox =
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
                          (targetToSubmission address (\x -> UpdateBox state.equations box.id x))
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

eqToHtml : Signal.Address QAction -> QState -> Equation -> Html
eqToHtml address state eq =
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
          , boxToHtml address state i.input
          ]

view : Signal.Address QAction -> QState -> Html
view address state =
  let
    fullEqToHtml = eqToHtml address state
  in
    Html.div
          [ Html.Attributes.class "blanks" ]
          (List.map fullEqToHtml state.equations)


-- view : Signal.Address Action -> State -> Html
-- view address state =
--   let
--     eqs = List.map (view (Signal.forward address RelayQuestion)) state.system
--                 [ Html.div
--                         []
--                         [ makeMath (s1 ++ " + x = " ++ s2) ]
--                 , Html.div
--                         []
--                         [ makeMath ("     x = " ++ s2 ++ " - " ++ s1) ]
--                 , Html.div
--                         []
--                         [ Html.text "x  = "
--                         , Html.input
--                                 [ Html.Attributes.type' "text"
--                                 , completionClass question "x" |> Html.Attributes.class
--                                 , Html.Events.on "change"
--                                       Html.Events.targetValue
--                                             (targetToSubmission address question.id "x")
--                                 ]
--                                 [ ]
--                         , Html.button
--                                 [ "btn btn-side " ++ completionClass question "x" |> Html.Attributes.class ]
--                                 [ Html.node "i"
--                                         [ Html.Attributes.class (faClass question "x") ]
--                                         [ ]
--                                 ]
--                         ]
