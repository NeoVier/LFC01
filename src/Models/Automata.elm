module Models.Automata exposing (..)

import Models.Alphabet as Alphabet
import Models.State as State
import Models.Transition as Transition


type Automaton
    = FiniteDeterministic AFD
    | FiniteNonDeterministic AFND


type alias AFD =
    { states : List State.State
    , initialState : State.State
    , finalStates : List State.State
    , alphabet : Alphabet.DeterministicAlphabet
    , transitions : List Transition.DeterministicTransition
    }


type alias AFND =
    { states : List State.State
    , initialState : State.State
    , finalStates : List State.State
    , alphabet : Alphabet.NonDeterministicAlphabet
    , transitions : List Transition.NonDeterministicTransition
    }
