module View.Automata.NonDeterministic exposing (viewAFND)

import Html exposing (..)
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import View.Styles exposing (..)


viewAFND : Automata.AFND -> Html msg
viewAFND afnd =
    text ""


viewNonDeterministicTransition :
    Transition.NonDeterministicTransition
    -> Html msg
viewNonDeterministicTransition transition =
    td tableItemStyles
        (List.map
            (\next ->
                case next of
                    State.Dead ->
                        text "-"

                    State.Valid nextLabel ->
                        text (nextLabel ++ " ")
            )
            transition.nextStates
        )
