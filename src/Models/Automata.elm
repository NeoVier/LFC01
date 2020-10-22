module Models.Automata exposing (..)

import Models.Alphabet as Alphabet
import Models.State as State
import Models.Transition as Transition


type alias AFD =
    { states : List State.State
    , initialState : State.State
    , finalStates : List State.State
    , alphabet : Alphabet.Alphabet
    , transitions : Transition.DeterministicTransition
    }
