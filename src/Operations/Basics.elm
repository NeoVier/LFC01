{-
   Operations/Basics.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to execute basic operations on AFDs, such as
   union and intersection
-}


module Operations.Basics exposing (..)

import Conversion.Automata as CAutomata
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils



-- Performs the union operation with two AFDs


union : Automata.AFD -> Automata.AFD -> Automata.AFND
union afd1 afd2 =
    let
        newAlphabet =
            Alphabet.NDA
                (Utils.joinDeterministicAlphabets afd1.alphabet afd2.alphabet)
                Alphabet.Epsilon

        afnd1 =
            generateNewAFNDForUnion afd1 True newAlphabet

        afnd2 =
            generateNewAFNDForUnion afd2 False newAlphabet

        newInitialState =
            State.Valid "unionInitial"

        initialTransition =
            { prevState = newInitialState
            , nextStates = [ afnd1.initialState, afnd2.initialState ]
            , conditions = Transition.WithEpsilon []
            }

        newFinalStates =
            afnd1.finalStates ++ afnd2.finalStates

        newStates =
            newInitialState
                :: (afnd1.states ++ afnd2.states)

        newTransitions =
            initialTransition
                :: (afnd1.transitions ++ afnd2.transitions)
    in
    { states = newStates
    , initialState = newInitialState
    , finalStates = newFinalStates
    , alphabet = newAlphabet
    , transitions = newTransitions
    }



-- Generates an AFND so we can diferentiate from the two union automata


generateNewAFNDForUnion :
    Automata.AFD
    -> Bool
    -> Alphabet.NonDeterministicAlphabet
    -> Automata.AFND
generateNewAFNDForUnion afd first alphabet =
    let
        prefix =
            if first then
                "1u"

            else
                "2u"
    in
    { states =
        List.map
            (Utils.addPrefix prefix)
            afd.states
    , initialState = Utils.addPrefix prefix afd.initialState
    , finalStates = List.map (Utils.addPrefix prefix) afd.finalStates
    , alphabet = alphabet
    , transitions =
        List.map (generateNewTransitionForUnion prefix) afd.transitions
    }



-- Generates new transitions for the new union automata


generateNewTransitionForUnion :
    String
    -> Transition.DeterministicTransition
    -> Transition.NonDeterministicTransition
generateNewTransitionForUnion prefix transition =
    { prevState = Utils.addPrefix prefix transition.prevState
    , nextStates = [ Utils.addPrefix prefix transition.nextState ]
    , conditions = Transition.NoEpsilon transition.conditions
    }



-- Performs the intersection operation with two AFDs
-- TODO


intersection : Automata.AFD -> Automata.AFD -> Automata.AFD
intersection afd1 afd2 =
    afd1
