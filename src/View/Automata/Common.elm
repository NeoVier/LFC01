{-
   View/Automata/Common.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains common functions to view automata
-}


module View.Automata.Common exposing (viewAutomataHeader)

import Html exposing (..)
import Models.Alphabet as Alphabet
import Utils.Utils as Utils
import View.Styles exposing (..)



-- Given an alphabet, returns a table entry


viewAutomataHeader : Alphabet.Alphabet -> Html msg
viewAutomataHeader alphabet =
    tr tableRowStyles
        ([ th tableItemStyles [ text "ẟ" ] ] ++ alphabetHeader alphabet)



-- Helper function to assemble the table entry


alphabetHeader : Alphabet.Alphabet -> List (Html msg)
alphabetHeader alphabet =
    case alphabet of
        Alphabet.Deterministic symbols ->
            List.sortWith Utils.compareAlphabetSymbols symbols
                |> List.map
                    (\symbol ->
                        th tableItemStyles
                            [ text (Utils.symbolToString symbol) ]
                    )

        Alphabet.NonDeterministic ndalphabet ->
            case ndalphabet of
                Alphabet.NDA symbols epsilon ->
                    (List.sortWith Utils.compareAlphabetSymbols symbols
                        |> List.map
                            (\symbol ->
                                th tableItemStyles
                                    [ text
                                        (Utils.symbolToString
                                            symbol
                                        )
                                    ]
                            )
                    )
                        ++ [ th tableItemStyles [ text "ε" ] ]
