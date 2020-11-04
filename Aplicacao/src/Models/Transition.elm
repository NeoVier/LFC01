{-
   Models/Transition.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the definition of everything related to Transitions
-}


module Models.Transition exposing (..)

import Models.Alphabet as Alphabet
import Models.State as State



-- General Transition type


type Transition
    = Deterministic DeterministicTransition
    | NonDeterministic NonDeterministicTransition



-- A Deterministic Transition has a previous state, a next state and a list of
-- conditions (can be interpreted as joined by an OR)


type alias DeterministicTransition =
    { prevState : State.State
    , nextState : State.State
    , conditions : DeterministicConditions
    }



-- A Non Deterministic Transition has a previous state, a list of next states it
-- can reach, and a list of conditions (can be interpreted as joined by an OR)


type alias NonDeterministicTransition =
    { prevState : State.State
    , nextStates : List State.State
    , conditions : NonDeterministicConditions
    }



-- Deterministic Conditions are made up of Symbols


type alias DeterministicConditions =
    List Alphabet.Symbol



-- A Non Deterministic Condition can have Epsilon or not


type NonDeterministicConditions
    = NoEpsilon DeterministicConditions
    | WithEpsilon DeterministicConditions
