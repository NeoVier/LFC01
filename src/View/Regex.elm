{-
   View/Regex.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to view Regular Expressions
-}


module View.Regex exposing (..)

import Html exposing (..)
import Models.Alphabet as Alphabet
import Models.Regex as Regex
import Utils.Utils as Utils
import View.Styles as Styles



{- Turn a list of IdRegex into HTML -}


viewIdRegexes : List Regex.IdRegex -> Html msg
viewIdRegexes =
    List.map viewRegexId >> div []



{- Turn an IdRegex into HTML -}


viewRegexId : Regex.IdRegex -> Html msg
viewRegexId idRegex =
    case idRegex of
        ( id, regex ) ->
            div []
                [ h3 [] [ text <| id ++ ": " ++ viewRegex regex ]
                ]



{- Turn a Regex into a string -}


viewRegex : Regex.Regex -> String
viewRegex r =
    case r of
        Regex.Epsilon ->
            "ε"

        Regex.Symbol s ->
            case s of
                Alphabet.Single _ ->
                    Utils.symbolToString s

                Alphabet.Group _ ->
                    "[" ++ Utils.symbolToString s ++ "]"

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
