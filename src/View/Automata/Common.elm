module View.Automata.Common exposing (..)

import Html exposing (..)
import Models.Alphabet as Alphabet
import View.Styles exposing (..)


viewAutomataHeader : Alphabet.Alphabet -> Html msg
viewAutomataHeader alphabet =
    tr tableRowStyles
        ([ th tableItemStyles [ text "Estados" ] ] ++ alphabetHeader alphabet)


alphabetHeader : Alphabet.Alphabet -> List (Html msg)
alphabetHeader alphabet =
    case alphabet of
        Alphabet.Deterministic symbols ->
            List.sort symbols
                |> List.map (\symbol -> th tableItemStyles [ text symbol ])

        Alphabet.NonDeterministic ndalphabet ->
            case ndalphabet of
                Alphabet.NDA symbols epsilon ->
                    (List.sort symbols
                        |> List.map
                            (\symbol ->
                                th tableItemStyles [ text symbol ]
                            )
                    )
                        ++ [ th tableItemStyles [ text "&" ] ]
