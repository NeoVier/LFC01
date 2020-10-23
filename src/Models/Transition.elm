module Models.Transition exposing (..)

import Models.Alphabet as Alphabet
import Models.State as State


type Transition
    = Deterministic DeterministicTransition
    | NonDeterministic NonDeterministicTransition


type alias DeterministicTransition =
    { prevState : State.State
    , nextState : State.State
    , conditions : List Alphabet.Symbol
    }


type alias NonDeterministicTransition =
    { prevState : State.State
    , nextStates : List State.State
    , conditions : NonDeterministicConditions
    }


type NonDeterministicConditions
    = NoEpsilon (List Alphabet.Symbol)
    | WithEpsilon (List Alphabet.Symbol)
