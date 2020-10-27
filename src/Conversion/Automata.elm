module Conversion.Automata exposing (afndToAfd)

import Conversion.Alphabet as CAlphabet
import Conversion.Transition as CTransition
import Models.Automata as Automata
import Models.State as State
import Utils.Utils as Utils



-- AUTOMATA


afndToAfd : Automata.AFND -> Automata.AFD
afndToAfd afnd =
    let
        -- TODO
        newTransitions =
            List.map CTransition.nonDeterministicToDeterministic
                afnd.transitions

        newStates =
            List.map (\transition -> transition.nextState) newTransitions
                |> List.filter (\state -> not (List.member state afnd.states))

        newFinalStates =
            List.filter
                (\state ->
                    case state of
                        State.Dead ->
                            False

                        State.Valid labels ->
                            String.split ", " labels
                                |> List.any
                                    (\label ->
                                        List.member (State.Valid label)
                                            afnd.finalStates
                                    )
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
