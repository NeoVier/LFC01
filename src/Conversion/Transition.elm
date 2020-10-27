module Conversion.Transition exposing (nonDeterministicToDeterministic)

import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils


nonDeterministicToDeterministic :
    Transition.NonDeterministicTransition
    -> Transition.DeterministicTransition
nonDeterministicToDeterministic ndt =
    { prevState = ndt.prevState
    , nextState = Utils.listOfStatesToState ndt.nextStates
    , conditions = nonDeterministicToDeterministicConditions ndt.conditions
    }


nonDeterministicToDeterministicConditions :
    Transition.NonDeterministicConditions
    -> Transition.DeterministicConditions
nonDeterministicToDeterministicConditions ndc =
    case ndc of
        Transition.NoEpsilon conditions ->
            conditions

        -- TODO
        Transition.WithEpsilon conditions ->
            conditions


joinParentTransitions :
    Automata.AFND
    -> State.State
    -> List Transition.NonDeterministicTransition
joinParentTransitions afnd compositeState =
    let
        parentStates : List State.State
        parentStates =
            Utils.stateToListOfStates compositeState

        parentTransitions : List Transition.NonDeterministicTransition
        parentTransitions =
            List.concatMap
                (Utils.getOutTransitionsNonDeterministic afnd)
                parentStates

        groups =
            Utils.groupByConditions parentTransitions
    in
    []
