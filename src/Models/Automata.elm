{-
   Models/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the definition of everything related to Automata
-}


module Models.Automata exposing (..)

import Models.Alphabet as Alphabet
import Models.State as State
import Models.Transition as Transition



{- General Automaton type -}


type Automaton
    = FiniteDeterministic AFD
    | FiniteNonDeterministic AFND



{- Deterministic Finite Automaton is a 5-tuple of states, initial state, final
   states, alphabet and transitions
-}


type alias AFD =
    { states : List State.State
    , initialState : State.State
    , finalStates : List State.State
    , alphabet : Alphabet.DeterministicAlphabet
    , transitions : List Transition.DeterministicTransition
    }



{- Non Deterministic Finite Automaton is a 5-tuple of states, initial state,
   final states, alphabet and transitions
-}


type alias AFND =
    { states : List State.State
    , initialState : State.State
    , finalStates : List State.State
    , alphabet : Alphabet.NonDeterministicAlphabet
    , transitions : List Transition.NonDeterministicTransition
    }
