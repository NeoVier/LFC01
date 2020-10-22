module View.Automata.Automata exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Types.Types as Types
import Utils.Utils as Utils
import View.Styles exposing (..)


viewCurrentAutomaton : Types.Model -> Html Types.Msg
viewCurrentAutomaton model =
    case model.currentAutomaton of
        Nothing ->
            h1 [] [ text "Nenhum autômato selecionado" ]

        Just automaton ->
            case automaton of
                Automata.FiniteDeterministic afd ->
                    viewAFD afd

                Automata.FiniteNonDeterministic afnd ->
                    h1 [] [ text "Still not supported" ]


viewAFD : Automata.AFD -> Html Types.Msg
viewAFD afd =
    table tableStyles
        (viewAFDHeader afd :: getAFDRows afd)


viewAFDHeader : Automata.AFD -> Html Types.Msg
viewAFDHeader afd =
    tr tableRowStyles
        ([ th tableItemStyles [ text "Estados" ] ]
            ++ alphabetAsHeader afd.alphabet
        )


alphabetAsHeader : Alphabet.Alphabet -> List (Html Types.Msg)
alphabetAsHeader alphabet =
    List.map (\symbol -> th tableItemStyles [ text symbol ]) (List.sort alphabet.symbols)


getAFDRows : Automata.AFD -> List (Html Types.Msg)
getAFDRows afd =
    List.map (\state -> getStateRow afd state) afd.states


getStateRow : Automata.AFD -> State.State -> Html Types.Msg
getStateRow afd prevState =
    case prevState of
        State.Dead ->
            text ""

        State.Valid label ->
            let
                isInitial =
                    prevState == afd.initialState

                isFinal =
                    List.member prevState afd.finalStates

                prefix =
                    if isInitial && isFinal then
                        "->* "

                    else if isInitial then
                        "-> "

                    else if isFinal then
                        "* "

                    else
                        ""

                transitions =
                    Utils.getFlatOutTransitionsDeterministic afd prevState
                        |> Utils.sortTransitionsDeterministic
            in
            tr tableRowStyles
                (td []
                    [ text (prefix ++ label) ]
                    :: List.map
                        (\transition ->
                            case transition.nextState of
                                State.Dead ->
                                    td tableItemStyles [ text "-" ]

                                State.Valid nextLabel ->
                                    td tableItemStyles [ text nextLabel ]
                        )
                        transitions
                )
