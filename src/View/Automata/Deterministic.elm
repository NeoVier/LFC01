module View.Automata.Deterministic exposing (viewAFD)

import Html exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils
import View.Automata.Common as VC
import View.Styles exposing (..)


viewAFD : Automata.AFD -> Html msg
viewAFD afd =
    table tableStyles
        (VC.viewAutomataHeader (Alphabet.Deterministic afd.alphabet)
            :: getAutomatonRows afd
        )


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
                            viewFlatDeterministicTransition transition
                        )
                        transitions
                )


viewFlatDeterministicTransition :
    Transition.DeterministicTransition
    -> Html msg
viewFlatDeterministicTransition transition =
    case transition.nextState of
        State.Dead ->
            td tableItemStyles [ text "-" ]

        State.Valid nextLabel ->
            td tableItemStyles
                [ text nextLabel ]
