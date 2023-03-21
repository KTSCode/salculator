module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { yearsMarried : String
  , sarahSalary : String
  , kyleSalary : String
  }


init : Model
init =
  { yearsMarried = "5"
  , sarahSalary = ""
  , kyleSalary = ""
  }

-- UPDATE


type Msg
  = ChangeYearsMarried String
  | ChangeSarahSalary String
  | ChangeKyleSalary String


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeYearsMarried val ->
      { model | yearsMarried = val }

    ChangeSarahSalary val ->
      { model | sarahSalary = val }

    ChangeKyleSalary val ->
      { model | kyleSalary = val }



-- VIEW
toFloatDef : String -> Float
toFloatDef str =
  Maybe.withDefault 0 (String.toFloat str)

calculateTithe : String -> String -> String -> String
calculateTithe yearsMarried sarahSalary kyleSalary =
  String.fromFloat (
    (
      ((toFloatDef sarahSalary) + (toFloatDef kyleSalary)) / 12) / ((toFloatDef yearsMarried) + 10))

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Years Married", value model.yearsMarried, onInput ChangeYearsMarried ] []
      , input [ placeholder "Sarah Salary", value model.sarahSalary, onInput ChangeSarahSalary ] []
      , input [ placeholder "Kyle Salary", value model.kyleSalary, onInput ChangeKyleSalary ] []
    , div [] [ text (calculateTithe model.yearsMarried model.sarahSalary model.kyleSalary) ]
    ]
