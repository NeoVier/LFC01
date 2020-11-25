{-
   Operations/Conversion/Transition.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to convert between Transitions
-}


module Operations.Conversion.Transition exposing (nonDeterministicToDeterministic)

import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils



{- Converts a non deterministic transition to a deterministic transition -}


nonDeterministicToDeterministic :
    Transition.NonDeterministicTransition
    -> Transition.DeterministicTransition
nonDeterministicToDeterministic ndt =
    { prevState = ndt.prevState
    , nextState = Utils.listOfStatesToState ndt.nextStates
    , conditions = nonDeterministicToDeterministicConditions ndt.conditions
    }



{- Converts non deterministic conditions to deterministic conditions -}


nonDeterministicToDeterministicConditions :
    Transition.NonDeterministicConditions
    -> Transition.DeterministicConditions
nonDeterministicToDeterministicConditions ndc =
    case ndc of
        Transition.NoEpsilon conditions ->
            conditions

        Transition.WithEpsilon conditions ->
            conditions
