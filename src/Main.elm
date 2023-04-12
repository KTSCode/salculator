module Main exposing (..)

import Browser
import Basics exposing(..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
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
      { model | yearsMarried = (filterInput val) }

    ChangeSarahSalary val ->
      { model | sarahSalary = (filterInput val) }

    ChangeSarahCont val ->
      { model | sarahCont = (filterInput val) }

    ChangeKyleSalary val ->
      { model | kyleSalary = (filterInput val) }

    ChangeKyleCont val ->
      { model | kyleCont = (filterInput val) }

-- VIEW
toFloatDef : String -> Float
toFloatDef str =
  Maybe.withDefault 0 (String.toFloat str)

formatNumber : Float -> String
formatNumber number =
  number
  |> abs
  |> floor
  |> String.fromInt
  |> String.reverse
  |> String.toList
  |> List.indexedMap
    (\index char -> if (Basics.modBy (index + 1) 3) == 0 then [char, ','] else [char] )
  |> List.concatMap identity
  |> List.reverse
  |> String.fromList
  |> (\str -> if number < 0 then "-" ++ str else str)

formatCurrency : Float -> String
formatCurrency amount =
  abs amount
  |> (*) 100
  |> Basics.round
  |> Basics.toFloat
  |> (\x -> x / 100)
  |> String.fromFloat
  |> (\str -> "$" ++ str)
  |> String.padLeft 2 '0'
  |> (\str -> if amount < 0 then "-" ++ str else str)

filterInput : String -> String
filterInput str =
  str
  |> String.toList
  |> List.filter (\c -> Char.isDigit c || c == '.')
  |> String.fromList

reduceByPercent : Float -> Float -> Float
reduceByPercent toReduce percent =
  (toReduce - ((toReduce * (percent / 100))))

--monthlySalary : String
calculateTithe : String -> String -> String -> String -> String -> Float
calculateTithe yearsMarried sarahSalary sarahCont kyleSalary kyleCont =
  (reduceByPercent (toFloatDef sarahSalary) (toFloatDef sarahCont))
  |> (+) (reduceByPercent (toFloatDef kyleSalary) (toFloatDef kyleCont))
  |> (\x -> x / 12)
  |> (\x -> x * (((toFloatDef yearsMarried) + 10) / 100))

salaryToHourly : String -> Float
salaryToHourly str =
  (toFloatDef str) / 2080

--HTML functions

labeledValue : String -> String -> Html Msg
labeledValue label value =
    Html.div [ Attr.style "display" "flex"
             , Attr.style "flex-direction" "column"
             , Attr.style "align-items" "center"
             ][ Html.label [] [ text label ]
              , Html.br [] []
              , Html.label [Attr.style "color" "#55EFC4", Attr.style "marginBottom" "5px"] [ text value ]
              , Html.br [] []
              ]

formInput : String -> String -> (String -> Msg) -> Html Msg
formInput ph val oi =
  div [] [ input [ placeholder ph
                 , value val
                 , onInput oi
                 , Attr.style "backgroundColor" "#485460"
                 , Attr.style "color" "#ECF0F1"
                 , Attr.style "margin" "10px"
                 ] []
         , br [] []
         , br [] []
         ]

view : Model -> Html Msg
view model = div [Attr.id "app"
  , Attr.style "display" "flex"
  , Attr.style "flex-direction" "row"
  , Attr.style "justify-content" "space-around"
  , Attr.style "align-items" "top"
  , Attr.style "height" "100vh"
  , Attr.style "margin" "0"
  , Attr.style "font-family" "Roboto, sans-serif"
  , Attr.style "backgroundColor" "#1E272E"
  , Attr.style "color" "#ECF0F1"
  ][Html.form
    [ Attr.style "display" "flex"
    , Attr.style "flex-direction" "column"
    , Attr.style "align-items" "center"
    ]
    [ h2 [] [text "Input Data:"]
    , label [] [ text "Number of Years Married" ]
    , formInput "Years Married" model.yearsMarried ChangeYearsMarried
    , label [] [ text "Sarah's Salary" ]
    , formInput "Sarah's Salary" model.sarahSalary ChangeSarahSalary
    , label [] [ text "Sarah's Contribution %" ]
    , formInput "Sarah's Contribution" model.sarahCont ChangeSarahCont
    , label [] [ text "Kyle's Salary" ]
    , formInput "Kyle's Salary" model.kyleSalary ChangeKyleSalary
    , label [] [ text "Kyle's Contribution %" ]
    , formInput "Kyle's Contribution" model.kyleCont ChangeKyleCont
  ]
  , div [Attr.style "display" "flex"
    , Attr.style "flex-direction" "column"
    , Attr.style "align-items" "center"
  ][
    h2 [] [ text "Output Data:" ]
      , labeledValue "Monthly Tithe:" (formatCurrency (calculateTithe "0" model.sarahSalary model.sarahCont model.kyleSalary model.kyleCont))
      , labeledValue "Monthly Tithe + Giving:" (formatCurrency (calculateTithe model.yearsMarried model.sarahSalary model.sarahCont model.kyleSalary model.kyleCont))
      , labeledValue "Sarah Hourly Salary:" (formatCurrency ((salaryToHourly model.sarahSalary)))
      , labeledValue "Kyle Hourly Salary:" (formatCurrency ((salaryToHourly model.kyleSalary)))
  ]]


