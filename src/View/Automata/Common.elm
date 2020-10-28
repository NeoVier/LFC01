{-
   View/Automata/Common.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains common functions to view automata
-}


module View.Automata.Common exposing (viewAutomataHeader)

import Html exposing (..)
import Models.Alphabet as Alphabet
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
            List.sort symbols
                |> List.map
                    (\symbol ->
                        th tableItemStyles
                            [ text (String.fromChar symbol) ]
                    )

        Alphabet.NonDeterministic ndalphabet ->
            case ndalphabet of
                Alphabet.NDA symbols epsilon ->
                    (List.sort symbols
                        |> List.map
                            (\symbol ->
                                th tableItemStyles [ text (String.fromChar symbol) ]
                            )
                    )
                        ++ [ th tableItemStyles [ text "ε" ] ]
