module Utils.Utils exposing (..)

import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition


getOutTransitionsDeterministic : Automata.AFD -> State.State -> List Transition.DeterministicTransition
getOutTransitionsDeterministic afd prevState =
    List.filter
        (\transition -> transition.prevState == prevState)
        afd.transitions
