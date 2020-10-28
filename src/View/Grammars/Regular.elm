module View.Grammars.Regular exposing (viewGR)

import Html exposing (..)
import Models.Grammars as Grammars


viewGR : Grammars.Grammar -> Html msg
viewGR gr =
    List.map viewGRProduction gr.productions
        |> String.join "\n"
        |> text


viewGRProduction : Grammars.Production -> String
viewGRProduction production =
    String.fromChar production.fromSymbol
        ++ " -> "
        ++ (List.map viewGRProductionBody production.productions
                |> String.join " | "
           )


viewGRProductionBody : Grammars.ProductionBody -> String
viewGRProductionBody body =
    let
        consumed : String
        consumed =
            Maybe.map String.fromChar body.consumed
                |> Maybe.withDefault ""

        toSymbol =
            Maybe.map String.fromChar body.toSymbol
                |> Maybe.withDefault ""
    in
    consumed ++ toSymbol
