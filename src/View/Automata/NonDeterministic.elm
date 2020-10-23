module View.Automata.NonDeterministic exposing (viewAFND)

import Html exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils
import View.Automata.Common as VC
import View.Styles exposing (..)


viewAFND : Automata.AFND -> Html msg
viewAFND afnd =
    table tableStyles
        (VC.viewAutomataHeader (Alphabet.NonDeterministic afnd.alphabet)
            :: getAutomatonRows afnd
        )


getAutomatonRows : Automata.AFND -> List (Html msg)
getAutomatonRows afnd =
    List.map (\state -> getStateRow afnd state) afnd.states


getStateRow : Automata.AFND -> State.State -> Html msg
getStateRow afnd prevState =
    case prevState of
        State.Dead ->
            text ""

        State.Valid label ->
            let
                isInitial =
                    prevState == afnd.initialState

                isFinal =
                    List.member prevState afnd.finalStates

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
                    Utils.getFlatOutTransitionsNonDeterministic
                        afnd
                        prevState
                        |> Utils.sortTransitionsNonDeterministic
                        |> Utils.swapFirstAndLast
            in
            tr tableRowStyles
                (td [] [ text (prefix ++ label) ]
                    :: List.map
                        (\transition ->
                            viewFlatNonDeterministicTransition transition
                        )
                        transitions
                )


viewFlatNonDeterministicTransition :
    Transition.NonDeterministicTransition
    -> Html msg
viewFlatNonDeterministicTransition transition =
    if transition.nextStates == [ State.Dead ] then
        td tableItemStyles [ text "-" ]

    else
        td tableItemStyles
            [ List.map
                (\state ->
                    case state of
                        State.Dead ->
                            ""

                        State.Valid label ->
                            label
                )
                transition.nextStates
                |> List.filter (\x -> x /= "")
                |> String.join ", "
                |> text
            ]
