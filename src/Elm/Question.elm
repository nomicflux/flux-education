module Question where

import List
import Html exposing (Html)
import Maybe exposing (withDefault)

type alias QuestionID = Int

type alias BoxID = String

type alias InputBox =
  { completed : Bool
  , attempted : Bool
  , validation : Int -> Maybe Bool
  , guess : Maybe Int
  , id : BoxID
  }

type alias Question =
  { nums : List Int
  , boxes : List InputBox
  , id : QuestionID
  }

completionClass : Question -> BoxID -> String
completionClass question bid =
  let
    mbox = List.head (List.filter (\ b -> b.id == bid) question.boxes)
  in
    case mbox of
      Nothing -> ""
      Just box ->
        if box.completed
        then
          "completed"
        else
          if box.attempted
          then
            "incorrect"
          else
            "new-question"

faClass : Question -> BoxID -> String
faClass question bid =
  let
    mbox = List.head (List.filter (\ b -> b.id == bid) question.boxes)
  in
    case mbox of
      Nothing -> ""
      Just box ->
        if box.completed
        then
          "fa fa-check-square-o"
        else
          if box.attempted
          then
            "fa fa-square-o"
          else
            "fa fa-square-o"

mkBox : BoxID -> (Int -> Maybe Bool) -> InputBox
mkBox bid val = { completed = False
                , attempted = False
                , validation = val
                , guess = Nothing
                , id = bid
                }

mkQuestion : List (BoxID, (List Int) -> Int -> Maybe Bool) -> QuestionID -> List Int -> Question
mkQuestion validations qid nums =
  let
    fullValidations : List (BoxID, Int -> Maybe Bool)
    fullValidations = List.map (\ (var, f) -> (var, f nums)) validations
  in
    { nums = nums
    , boxes = List.map (uncurry mkBox) fullValidations
    , id = qid + 1
    }

mkQBatch : List (BoxID, (List Int) -> Int -> Maybe Bool) -> List (List Int) -> List Question
mkQBatch validations nums =
  List.indexedMap
      (mkQuestion validations)
      nums

validateBox : InputBox -> Int -> Bool
validateBox box guess = withDefault False (box.validation guess)

updateBox : BoxID -> Int -> InputBox -> InputBox
updateBox wantedBid val box =
  if box.id == wantedBid
  then
    { box | attempted = True, completed = validateBox box val, guess = Just val }
  else
    box

updateQuestion : ( QuestionID, BoxID ) -> Int -> Question -> Question
updateQuestion (qid, bid) val question =
  if question.id == qid
  then
    { question | boxes = List.map (updateBox bid val) question.boxes }
  else
    question

boxesComplete : Question -> Bool
boxesComplete q = List.all (\ b -> b.completed ) q.boxes

findLatestAnswered : List Question -> QuestionID
findLatestAnswered qs = qs
                      |> List.filter boxesComplete
                      |> List.map (.id)
                      |> List.maximum
                      |> Maybe.withDefault 0

makeMath : String -> Html
makeMath s = Html.math [] [ Html.text s ]
