{-
   View/Regex.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to view Regular Expressions
-}


module View.Regex exposing (..)

import Html exposing (..)
import Models.Regex as Regex
import Utils.Utils as Utils
import View.Styles as Styles


viewIdRegexes : List Regex.IdRegex -> Html msg
viewIdRegexes =
    List.map viewRegexId >> div []


viewRegexId : Regex.IdRegex -> Html msg
viewRegexId idRegex =
    case idRegex of
        ( id, regex ) ->
            div []
                [ h3 [] [ text <| id ++ ": " ++ viewRegex regex ]
                ]


viewRegex : Regex.Regex -> String
viewRegex r =
    case r of
        Regex.Epsilon ->
            "ε"

        Regex.Symbol s ->
            Utils.symbolToString s

        Regex.Union r1 r2 ->
            "(" ++ viewRegex r1 ++ " | " ++ viewRegex r2 ++ ")"

        Regex.Concat r1 r2 ->
            viewRegex r1 ++ viewRegex r2

        Regex.Star r1 ->
            "(" ++ viewRegex r1 ++ "*" ++ ")"

        Regex.Plus r1 ->
            "(" ++ viewRegex r1 ++ "+" ++ ")"

        Regex.Question r1 ->
            "(" ++ viewRegex r1 ++ "?" ++ ")"
