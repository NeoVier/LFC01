module View.Automata.Deterministic exposing (viewAFD)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Types.Types as Types exposing (Msg)
import Utils.Utils as Utils
import View.Styles exposing (..)


viewAFD : Automata.AFD -> Html msg
viewAFD afd =
    table tableStyles
        (viewAutomataHeader afd.alphabet
            :: getAutomatonRows afd
        )


viewAutomataHeader : Alphabet.DeterministicAlphabet -> Html msg
viewAutomataHeader alphabet =
    tr tableRowStyles
        ([ th tableItemStyles [ text "Estados" ] ]
            ++ alphabetAsHeader alphabet
        )


alphabetAsHeader : Alphabet.DeterministicAlphabet -> List (Html msg)
alphabetAsHeader alphabet =
    List.map (\symbol -> th tableItemStyles [ text symbol ])
        (List.sort alphabet)


getAutomatonRows : Automata.AFD -> List (Html msg)
getAutomatonRows afd =
    List.map (\state -> getStateRow afd state) afd.states


getStateRow : Automata.AFD -> State.State -> Html msg
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
                    Utils.getFlatOutTransitionsDeterministic
                        afd
                        prevState
                        |> Utils.sortTransitionsDeterministic
            in
            tr tableRowStyles
                (td []
                    [ text (prefix ++ label) ]
                    :: List.map
                        (\transition ->
                            viewDeterministicTransition transition
                        )
                        transitions
                )


viewDeterministicTransition :
    Transition.DeterministicTransition
    -> Html msg
viewDeterministicTransition transition =
    case transition.nextState of
        State.Dead ->
            td tableItemStyles [ text "-" ]

        State.Valid nextLabel ->
            td tableItemStyles
                [ text nextLabel ]
