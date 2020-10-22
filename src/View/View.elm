module View.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Types.Types as Types
import Utils.Utils as Utils


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
        (viewAFDHeader afd :: getAFDRows afd)


viewAFDHeader : Automata.AFD -> Html Types.Msg
viewAFDHeader afd =
    tr []
        ([ th [] [ text "Estados" ] ]
            ++ alphabetAsHeader afd.alphabet
        )


alphabetAsHeader : Alphabet.Alphabet -> List (Html Types.Msg)
alphabetAsHeader alphabet =
    List.map (\symbol -> th [] [ text symbol ]) (List.sort alphabet.symbols)


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
            tr []
                (td []
                    [ text (prefix ++ label) ]
                    :: List.map
                        (\transition ->
                            case transition.nextState of
                                State.Dead ->
                                    td [] [ text "-" ]

                                State.Valid nextLabel ->
                                    td [] [ text nextLabel ]
                        )
                        transitions
                )
