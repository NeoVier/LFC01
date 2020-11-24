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



{- Styles for the entire application -}


globalStyles : List (Attribute msg)
globalStyles =
    [ style "font-family" "sans-serif" ]



{- Styles for the application title -}


titleStyles : List (Attribute msg)
titleStyles =
    [ style "text-align" "center", style "padding" "1rem 0 3rem 0" ]



{- Styles for the main area -}


mainAreaStyles : List (Attribute msg)
mainAreaStyles =
    [ style "display" "grid"
    , style "grid-template-columns" "1fr 1fr 1fr"
    , style "text-align" "center"
    ]



{- Styles for the sentence input area -}


sentenceInputStyles : List (Attribute msg)
sentenceInputStyles =
    [ style "width" "100%" ]



{- Styles for the valid sentence indicator -}


validSentenceStyles : List (Attribute msg)
validSentenceStyles =
    [ style "color" "green", style "text-align" "center" ]



{- Styles for the invalid sentence indicator -}


invalidSentenceStyles : List (Attribute msg)
invalidSentenceStyles =
    [ style "color" "red", style "text-align" "center" ]



{- Styles for the left panel -}


leftPanelStyles : List (Attribute msg)
leftPanelStyles =
    [ style "grid-column" "1"
    , style "height" "100%"
    , style "width" "100%"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    ]



{- Styles for the history area -}


historyViewStyles : List (Attribute msg)
historyViewStyles =
    [ style "display" "flex"
    , style "flex-direction"
        "column"
    , style "align-items" "center"
    ]



{- Styles for the history rows -}


historyViewRowStyles : List (Attribute msg)
historyViewRowStyles =
    [ style "margin-bottom" "1em"
    , style "display" "flex"
    , style "width" "100%"
    ]



{- Styles for the history items -}


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



{- Styles for the history delete button -}


historyViewDeleteStyles : List (Attribute msg)
historyViewDeleteStyles =
    [ style "padding" "1em 1em"
    , style "text-align" "center"
    , style "cursor" "pointer"
    , style "height" "100%"
    ]



{- Styles for the current automaton title -}


currentAutomatonTitleStyles : List (Attribute msg)
currentAutomatonTitleStyles =
    [ style "text-align" "center" ]



{- Styles for the current automaton -}


currentAutomatonStyles : List (Attribute msg)
currentAutomatonStyles =
    [ style "grid-column" "2"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "width" "100%"
    ]



{- Styles for the table -}


tableStyles : List (Attribute msg)
tableStyles =
    [ style "border" "1px solid black"
    , style "border-collapse" "collapse"
    , style "text-align" "center"
    ]



{- Styles for the table rows -}


tableRowStyles : List (Attribute msg)
tableRowStyles =
    [ style "border" "1px solid black" ]



{- Styles for the table items -}


tableItemStyles : List (Attribute msg)
tableItemStyles =
    [ style "border" "1px solid black", style "padding" "1em 2em" ]



{- Styles for the right panel area -}


rightPanelStyles : List (Attribute msg)
rightPanelStyles =
    [ style "grid-column" "3"
    , style "height" "100%"
    , style "width" "100%"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    ]



{- Styles for the right panel control container area -}


rightPanelControlContainerStyles : List (Attribute msg)
rightPanelControlContainerStyles =
    [ style "display" "flex"
    , style "flex-direction" "column"
    ]



{- Styles for the right panel buttons -}


rightPanelButtonStyles : List (Attribute msg)
rightPanelButtonStyles =
    [ style "padding" "1em 1.5em"
    , style "cursor" "pointer"
    , style "margin-bottom" "1em"
    ]



{- Styles for the grammar container -}


grammarContainerStyles : List (Attribute msg)
grammarContainerStyles =
    [ style "display" "flex"
    , style "flex-direction" "column"
    ]
