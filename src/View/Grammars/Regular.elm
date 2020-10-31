{-
   View/Grammars/Regular.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to view Regular Grammars
-}


module View.Grammars.Regular exposing (viewGR)

import Html exposing (..)
import Models.Grammars as Grammars
import View.Styles as Styles


viewGR : Grammars.Grammar -> Html msg
viewGR gr =
    let
        initialProductions =
            List.filter (\p -> p.fromSymbol == gr.initialSymbol) gr.productions

        finalProductions =
            List.filter (\p -> p.fromSymbol /= gr.initialSymbol) gr.productions
    in
    List.map
        (\production -> h3 [] [ text (viewGRProduction production) ])
        (initialProductions ++ finalProductions)
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
            String.fromChar body.consumed

        toSymbol =
            Maybe.map (\s -> " [ " ++ s ++ " ] ") body.toSymbol
                |> Maybe.withDefault ""
    in
    consumed ++ toSymbol
