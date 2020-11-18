{-
   View/Grammars/Regular.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to view Regular Grammars
-}


module View.Grammars.Regular exposing (viewGR)

import Html exposing (..)
import Models.Alphabet as Alphabet
import Models.Grammars as Grammars
import Utils.Utils as Utils
import View.Styles as Styles


viewGR : Grammars.RegularGrammar -> Html msg
viewGR gr =
    let
        initialProductions =
            List.filter (\p -> p.fromSymbol == gr.initialSymbol) gr.productions

        finalProductions =
            List.filter (\p -> p.fromSymbol /= gr.initialSymbol) gr.productions

        initialProductionsStrings =
            let
                defaultStrings =
                    List.map viewGRProduction initialProductions
            in
            if gr.acceptsEmpty then
                List.map (String.replace " -> " " -> ε | ") defaultStrings

            else
                defaultStrings

        finalProductionsStrings =
            List.map viewGRProduction finalProductions
    in
    List.map
        (\productionString -> h3 [] [ text productionString ])
        (initialProductionsStrings ++ finalProductionsStrings)
        |> div Styles.grammarContainerStyles


viewGRProduction : Grammars.Production -> String
viewGRProduction production =
    production.fromSymbol
        ++ " -> "
        ++ (List.map viewGRProductionBody production.productions
                |> String.join " | "
           )


viewGRProductionBody : Grammars.ProductionBody -> String
viewGRProductionBody body =
    let
        consumed =
            case body.consumed of
                Alphabet.Single _ ->
                    Utils.symbolToString body.consumed

                Alphabet.Group _ ->
                    "[" ++ Utils.symbolToString body.consumed ++ "]"

        toSymbol =
            Maybe.map (\s -> " { " ++ s ++ " } ") body.toSymbol
                |> Maybe.withDefault ""
    in
    consumed ++ toSymbol
