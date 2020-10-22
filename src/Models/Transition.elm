module Models.Transition exposing (..)

import Models.Alphabet as Alphabet
import Models.State as State


type alias DeterministicTransition =
    { prevState : State.State
    , nextState : State.State
    , condition : List Alphabet.Symbol
    }


type alias NonDeterministicTransition =
    { prevState : State.State
    , nextState : State.State
    , condition : List (Maybe Alphabet.Symbol)
    }
