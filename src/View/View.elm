module View.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Types.Types as Types


view : Types.Model -> Html Types.Msg
view model =
    case model.currentAutomaton of
        Nothing ->
            h1 [] [ text "Nenhum automato selecionado" ]

        Just automaton ->
            case automaton of
                Automata.FiniteDeterministic afd ->
                    viewAFD afd

                Automata.FiniteNonDeterministic afnd ->
                    h1 [] [ text "Still not supported" ]


viewAFD : Automata.AFD -> Html Types.Msg
viewAFD afd =
    table []
        [ tr []
            ([ th [] [ text "Estados" ] ] ++ alphabetAsHeader afd.alphabet)
        ]


alphabetAsHeader : Alphabet.Alphabet -> List (Html Types.Msg)
alphabetAsHeader alphabet =
    List.map (\symbol -> th [] [ text symbol.value ]) alphabet.symbols


getAFDRows : Automata.AFD -> List (Html Types.Msg)
getAFDRows afd =
    List.map (\state -> getStateRow afd state) afd.states


getStateRow : Automata.AFD -> State.State -> Html Types.Msg
getStateRow afd prevState =
    text ""
