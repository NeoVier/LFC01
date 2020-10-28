{-
   Conversion/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to convert between Automata
-}


module Conversion.Automata exposing (afndToAfd)

import Conversion.Alphabet as CAlphabet
import Conversion.Transition as CTransition
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils exposing (stateToListOfStates)



-- Converts an AFND to AFD


afndToAfd : Automata.AFND -> Automata.AFD
afndToAfd afnd =
    let
        newStates =
            Utils.subsequences afnd.states
                |> List.sortBy List.length
                |> List.map Utils.listOfStatesToState
                |> List.filter (\state -> not (List.member state afnd.states))

        -- Add Epsilon star
        newStatesTransitions =
            List.concatMap
                (\complexState ->
                    List.concatMap
                        (Utils.getEpsilonStar afnd)
                        (Utils.stateToListOfStates complexState)
                        |> List.concatMap
                            (\state ->
                                Utils.getOutTransitionsNonDeterministic afnd
                                    state
                            )
                        |> List.filter
                            (\transition ->
                                transition.nextStates /= [ State.Dead ]
                            )
                        |> Utils.groupByConditions
                        |> List.map
                            (\x ->
                                let
                                    newTransition =
                                        Utils.joinTransitionsWithSameCondition x
                                in
                                { newTransition | prevState = complexState }
                            )
                )
                newStates

        newTransitions =
            List.map CTransition.nonDeterministicToDeterministic
                (afnd.transitions ++ newStatesTransitions)
                |> Utils.sortTransitionsDeterministic

        newFinalStates =
            List.filter
                (\state ->
                    Utils.stateToListOfStates state
                        |> List.any (\st -> List.member st afnd.finalStates)
                )
                newStates

        newInitialState =
            Utils.getEpsilonStar afnd afnd.initialState
                |> Utils.listOfStatesToState
    in
    { states = List.append afnd.states newStates
    , initialState = newInitialState
    , finalStates = List.append afnd.finalStates newFinalStates
    , alphabet = CAlphabet.nonDeterministicToDeterministic afnd.alphabet
    , transitions = newTransitions
    }
