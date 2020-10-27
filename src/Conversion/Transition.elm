module Conversion.Transition exposing (nonDeterministicToDeterministic)

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
