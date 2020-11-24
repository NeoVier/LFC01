{-
   Saving/Grammars.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to save Grammars
-}


module Saving.Grammars exposing
    ( contextFreeGrammarToString
    , grammarToString
    , regularGrammarToString
    )

import Models.Alphabet as Alphabet
import Models.Grammars as Grammars



{- Transform a grammar into a string -}


grammarToString : Grammars.Grammar -> String
grammarToString grammar =
    case grammar of
        Grammars.Regular gr ->
            regularGrammarToString gr

        Grammars.ContextFree glc ->
            contextFreeGrammarToString glc



{- Transform a regular grammar into a string -}


regularGrammarToString : Grammars.RegularGrammar -> String
regularGrammarToString gr =
    let
        initialProduction =
            List.filter (.fromSymbol >> (==) gr.initialSymbol) gr.productions

        addEpsilon =
            String.replace " -> " " -> & | "

        otherProductions =
            List.filter (.fromSymbol >> (/=) gr.initialSymbol) gr.productions
    in
    case List.head initialProduction of
        Nothing ->
            ""

        Just f ->
            let
                firstProduction =
                    if gr.acceptsEmpty then
                        addEpsilon (productionToString f)

                    else
                        productionToString f
            in
            String.join "\n"
                (firstProduction
                    :: List.map productionToString otherProductions
                )



{- Transform a context free grammar into a string -}


contextFreeGrammarToString : Grammars.ContextFreeGrammar -> String
contextFreeGrammarToString glc =
    let
        initialProduction =
            List.filter (.fromSymbol >> (==) glc.initialSymbol) glc.productions

        nonInitialProductions =
            List.filter (.fromSymbol >> (/=) glc.initialSymbol) glc.productions
    in
    case List.head initialProduction of
        Nothing ->
            ""

        Just s ->
            contextFreeProductionToString s
                :: List.map contextFreeProductionToString nonInitialProductions
                |> String.join "\n"



{- Transform a symbol into a string -}


symbolToString : Alphabet.Symbol -> String
symbolToString s =
    case s of
        Alphabet.Single c ->
            String.fromChar c

        Alphabet.Group g ->
            "["
                ++ String.join ""
                    (List.map
                        (\( c1, c2 ) ->
                            String.fromList [ c1, '-', c2 ]
                        )
                        g
                    )
                ++ "]"



{- Transform a context free production into a string -}


contextFreeProductionToString : Grammars.ContextFreeProduction -> String
contextFreeProductionToString prod =
    prod.fromSymbol
        ++ " -> "
        ++ String.join " | "
            (List.map contextFreeProductionBodyToString prod.bodies)



{- Transform a context free production body into a string -}


contextFreeProductionBodyToString : Grammars.ContextFreeProductionBody -> String
contextFreeProductionBodyToString body =
    if List.isEmpty body then
        "&"

    else
        List.map
            (\item ->
                case item of
                    Grammars.Terminal symbol ->
                        symbolToString symbol

                    Grammars.NonTerminal symbol ->
                        symbol
            )
            body
            |> String.join ""



{- Transform a regular production into a string -}


productionToString : Grammars.Production -> String
productionToString prod =
    prod.fromSymbol
        ++ " -> "
        ++ String.join " | " (List.map productionBodyToString prod.productions)



{- Transform a regular production body into a string -}


productionBodyToString : Grammars.ProductionBody -> String
productionBodyToString body =
    case body.toSymbol of
        Nothing ->
            symbolToString body.consumed

        Just to ->
            symbolToString body.consumed ++ to
