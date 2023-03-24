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
  , sarahCont : String
  , kyleSalary : String
  , kyleCont : String
  }


init : Model
init =
  { yearsMarried = "5"
  , sarahSalary = ""
  , sarahCont = "6"
  , kyleSalary = ""
  , kyleCont = "4"
  }

-- UPDATE


type Msg
  = ChangeYearsMarried String
  | ChangeSarahSalary String
  | ChangeSarahCont String
  | ChangeKyleSalary String
  | ChangeKyleCont String


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeYearsMarried val ->
      { model | yearsMarried = val }

    ChangeSarahSalary val ->
      { model | sarahSalary = val }

    ChangeSarahCont val ->
      { model | sarahCont = val }

    ChangeKyleSalary val ->
      { model | kyleSalary = val }

    ChangeKyleCont val ->
      { model | kyleCont = val }


-- VIEW
toFloatDef : String -> Float
toFloatDef str =
  Maybe.withDefault 0 (String.toFloat str)

reduceByPercent : Float -> Float -> Float
reduceByPercent toReduce percent =
  (toReduce - ((toReduce * (percent / 100))))

--monthlySalary : String 
calculateTithe : String -> String -> String -> String -> String -> String
calculateTithe yearsMarried sarahSalary sarahCont kyleSalary kyleCont =
  (reduceByPercent (toFloatDef sarahSalary) (toFloatDef sarahCont))
  |> (\x -> x + (reduceByPercent (toFloatDef kyleSalary) (toFloatDef kyleCont)))
  |> (\x -> x / 12)
  |> (\x -> x * (((toFloatDef yearsMarried) + 10) / 100))
  |> String.fromFloat

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
      , label [] [ text "Sarah's Contribution %" ]
      , br [] []
      , input [ placeholder "Sarah Cont", value model.sarahCont, onInput ChangeSarahCont ] []
      , br [] []
      , label [] [ text "Kyle's Salary" ]
      , br [] []
      , input [ placeholder "Kyle Salary", value model.kyleSalary, onInput ChangeKyleSalary ] []
      , br [] []
      , label [] [ text "Kyle's Contribution %" ]
      , br [] []
      , input [ placeholder "Kyle Cont", value model.kyleCont, onInput ChangeKyleCont ] []
      , br [] []
      , label [] [ text "------------------------------" ]
      , br [] []
      , label [] [ text (String.concat [ "Monthly Tithe: $" , (calculateTithe "0" model.sarahSalary model.sarahCont model.kyleSalary model.kyleCont) ] ) ]
      , br [] []
      , label [] [ text (String.concat [ "Monthly Tithe + Giving: $" , (calculateTithe model.yearsMarried model.sarahSalary model.sarahCont model.kyleSalary model.kyleCont) ] ) ]
    ]
