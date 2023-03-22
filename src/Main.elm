module Main exposing (..)

import Browser
import Html exposing ( Attribute, Html, br, div, h1, input, label, text )
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
  Html.form []
    [ h1 [] [ text "Sarah's Salculator:" ]
      , label [] [ text "Number of Years Married" ]
      , br [] []
      ,input [ placeholder "Years Married", value model.yearsMarried, onInput ChangeYearsMarried ] []
      , br [] []
      , label [] [ text "Sarah's Salary" ]
      , br [] []
      , input [ placeholder "Sarah Salary", value model.sarahSalary, onInput ChangeSarahSalary ] []
      , br [] []
      , label [] [ text "Kyle's Salary" ]
      , br [] []
      , input [ placeholder "Kyle Salary", value model.kyleSalary, onInput ChangeKyleSalary ] []
      , br [] []
      , br [] []
      , label [] [ text (String.concat [ "Monthly Tithe: $" , (calculateTithe model.yearsMarried model.sarahSalary model.kyleSalary) ] ) ]
    ]
