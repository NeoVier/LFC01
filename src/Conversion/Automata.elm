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
                |> Debug.log "New states"

        newStatesTransitions : List Transition.NonDeterministicTransition
        newStatesTransitions =
            List.concatMap
                (\complexState ->
                    Utils.stateToListOfStates complexState
                        |> Debug.log "Analyzing states"
                        |> List.concatMap
                            (\state ->
                                Utils.getOutTransitionsNonDeterministic afnd state
                            )
                        |> Debug.log "All states transitions"
                        |> Utils.groupByConditions
                        |> Debug.log "Grouped"
                        |> List.map
                            (\x ->
                                let
                                    newTransition =
                                        Utils.joinTransitionsWithSameCondition x
                                in
                                { newTransition | prevState = complexState }
                            )
                        |> Debug.log "Joined"
                )
                newStates

        -- TODO
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
