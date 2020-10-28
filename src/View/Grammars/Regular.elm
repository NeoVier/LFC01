module View.Grammars.Regular exposing (viewGR)

import Html exposing (..)
import Models.Grammars as Grammars
import View.Styles as Styles


viewGR : Grammars.Grammar -> Html msg
viewGR gr =
    List.map
        (\production -> h3 [] [ text (viewGRProduction production) ])
        gr.productions
        |> div Styles.grammarContainerStyles


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
        consumed =
            String.fromChar body.consumed

        toSymbol =
            Maybe.map String.fromChar body.toSymbol
                |> Maybe.withDefault ""
    in
    consumed ++ toSymbol
