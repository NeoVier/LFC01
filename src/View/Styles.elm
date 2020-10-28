{-
   View/Styles.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains all the style definitions used in the application
-}


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


sentenceInputStyles : List (Attribute msg)
sentenceInputStyles =
    [ style "width" "100%" ]


validSentenceStyles : List (Attribute msg)
validSentenceStyles =
    [ style "color" "green", style "text-align" "center" ]


invalidSentenceStyles : List (Attribute msg)
invalidSentenceStyles =
    [ style "color" "red", style "text-align" "center" ]


leftPanelStyles : List (Attribute msg)
leftPanelStyles =
    [ style "grid-column" "1"
    , style "height" "100%"
    , style "width" "100%"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    ]


historyViewStyles : List (Attribute msg)
historyViewStyles =
    [ style "display" "flex"
    , style "flex-direction"
        "column"
    , style "align-items" "center"
    ]


historyViewRowStyles : List (Attribute msg)
historyViewRowStyles =
    [ style "margin-bottom" "1em"
    , style "display" "flex"
    , style "width" "100%"
    ]


historyViewItemStyles : List (Attribute msg)
historyViewItemStyles =
    [ style "padding" "1em 2em"
    , style "text-align" "center"
    , style "cursor" "pointer"
    , style "margin-bottom" "1em"
    , style "height" "100%"
    , style "margin-left" "0.5em"
    , style "width" "100%"
    ]


historyViewDeleteStyles : List (Attribute msg)
historyViewDeleteStyles =
    [ style "padding" "1em 1em"
    , style "text-align" "center"
    , style "cursor" "pointer"
    , style "height" "100%"
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


rightPanelStyles : List (Attribute msg)
rightPanelStyles =
    [ style "grid-column" "3"
    , style "height" "100%"
    , style "width" "100%"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    ]


rightPanelControlContainerStyles : List (Attribute msg)
rightPanelControlContainerStyles =
    [ style "display" "flex"
    , style "flex-direction" "column"
    ]


rightPanelButtonStyles : List (Attribute msg)
rightPanelButtonStyles =
    [ style "padding" "1em 1.5em"
    , style "cursor" "pointer"
    , style "margin-bottom" "1em"
    ]
