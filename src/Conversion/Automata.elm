module Conversion.Automata exposing (afndToAfd)

import Conversion.Alphabet as CAlphabet
import Conversion.Transition as CTransition
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils exposing (stateToListOfStates)



-- AUTOMATA


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
                    -- Utils.stateToListOfStates complexState
                    --     ++ List.concatMap
                    List.concatMap
                        (Utils.getEpsilonStar afnd)
                        (Utils.stateToListOfStates complexState)
                        |> Debug.log "States"
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
