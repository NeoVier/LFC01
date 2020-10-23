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



-- TEST


state0 : State.State
state0 =
    State.Valid "0"


state1 : State.State
state1 =
    State.Valid "1"


state2 : State.State
state2 =
    State.Valid "2"


state3 : State.State
state3 =
    State.Valid "3"


state4 : State.State
state4 =
    State.Valid "4"


initialState : State.State
initialState =
    state0


finalStates : List State.State
finalStates =
    [ state4 ]


alphabet : Alphabet.NonDeterministicAlphabet
alphabet =
    Alphabet.NDA [ "a", "b" ] Alphabet.Epsilon


transition0 : Transition.NonDeterministicTransition
transition0 =
    Transition.NonDeterministicTransition state0
        [ state1, state2 ]
        (Transition.NoEpsilon [ "a" ])


transition1 : Transition.NonDeterministicTransition
transition1 =
    Transition.NonDeterministicTransition state1
        [ state2 ]
        (Transition.NoEpsilon [ "b" ])


transition2 : Transition.NonDeterministicTransition
transition2 =
    Transition.NonDeterministicTransition state2
        [ state3 ]
        (Transition.NoEpsilon [ "a" ])


transition3 : Transition.NonDeterministicTransition
transition3 =
    Transition.NonDeterministicTransition state3
        [ state4 ]
        (Transition.WithEpsilon [ "b" ])


afnd0 : AFND
afnd0 =
    AFND
        [ state0, state1, state2, state3, state4 ]
        initialState
        finalStates
        alphabet
        [ transition0, transition1, transition2, transition3 ]
