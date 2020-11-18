module Saving.Grammars exposing (grammarToString)

import Models.Alphabet as Alphabet
import Models.Grammars as Grammars


grammarToString : Grammars.RegularGrammar -> String
grammarToString gr =
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


productionToString : Grammars.Production -> String
productionToString prod =
    prod.fromSymbol
        ++ " -> "
        ++ String.join " | " (List.map productionBodyToString prod.productions)


productionBodyToString : Grammars.ProductionBody -> String
productionBodyToString body =
    case body.toSymbol of
        Nothing ->
            symbolToString body.consumed

        Just to ->
            symbolToString body.consumed ++ to
