{-
   View/Grammars/ContextFree.elm
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains functions to view Context Free Grammars
-}


module View.Grammars.ContextFree exposing (viewGLC)

import Html exposing (..)
import Models.Alphabet as Alphabet
import Models.Grammars as Grammars
import Utils.Utils as Utils
import View.Styles as Styles


viewGLC : Grammars.ContextFreeGrammar -> Html msg
viewGLC =
    .productions
        >> List.map (\prod -> h3 [] [ text (viewGLCProduction prod) ])
        >> div Styles.grammarContainerStyles


viewGLCProduction : Grammars.ContextFreeProduction -> String
viewGLCProduction production =
    production.fromSymbol
        ++ " -> "
        ++ (List.map
                viewGLCProductionBody
                production.productions
                |> String.join " | "
           )


viewGLCProductionBody : Grammars.ContextFreeProductionBody -> String
viewGLCProductionBody body =
    case body of
        [] ->
            "ε"

        _ ->
            List.map viewContextFreeProductionItem body |> String.join ""


viewContextFreeProductionItem : Grammars.ContextFreeProductionItem -> String
viewContextFreeProductionItem item =
    case item of
        Grammars.Terminal terminal ->
            case terminal of
                Alphabet.Single _ ->
                    Utils.symbolToString terminal

                Alphabet.Group _ ->
                    "[" ++ Utils.symbolToString terminal ++ "]"

        Grammars.NonTerminal nonTerminal ->
            " { " ++ nonTerminal ++ " } "
