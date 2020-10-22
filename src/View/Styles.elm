module View.Styles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State


globalStyles : List (Attribute msg)
globalStyles =
    [ style "font-family" "sans-serif" ]


titleStyles : List (Attribute msg)
titleStyles =
    [ style "text-align" "center", style "padding" "1rem 0 3rem 0" ]


mainAreaStyles : List (Attribute msg)
mainAreaStyles =
    [ style "display" "grid"
    , style "grid-template-columns" "1fr 1fr 1fr"
    , style "place-items" "center"
    ]


currentAutomatonTitleStyles : List (Attribute msg)
currentAutomatonTitleStyles =
    [ style "text-align" "center" ]


currentAutomatonStyles : List (Attribute msg)
currentAutomatonStyles =
    [ style "grid-column" "2" ]


tableStyles : List (Attribute msg)
tableStyles =
    [ style "border" "1px solid black"
    , style "border-collapse" "collapse"
    , style "text-align" "center"
    ]


tableRowStyles : List (Attribute msg)
tableRowStyles =
    [ style "border" "1px solid black" ]


tableItemStyles : List (Attribute msg)
tableItemStyles =
    [ style "border" "1px solid black", style "padding" "1em 2em" ]
