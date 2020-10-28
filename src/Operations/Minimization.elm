{-
   Operations/Minimization.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to minimize automata
-}


module Operations.Minimization exposing (minimizeAFD)

import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils



-- Minimize an AFD


minimizeAFD : Automata.AFD -> Automata.AFD
minimizeAFD afd =
    let
        newStates =
            List.filter
                (\state ->
                    List.member state
                        (allReachableStates afd)
                        && List.member state (allAliveStates afd)
                )
                afd.states
                |> List.filter (\state -> state /= State.Dead)
    in
    { afd
        | states = newStates
        , transitions =
            List.map
                (\transition ->
                    if List.member transition.nextState newStates then
                        transition

                    else
                        { transition | nextState = State.Dead }
                )
                afd.transitions
    }



-- REACHABLE STATES
-- Gets directly reachable states from a state


reachableStates : Automata.AFD -> State.State -> List State.State
reachableStates afd state =
    Utils.getOutTransitionsDeterministic afd state
        |> List.map .nextState
        |> (::) state



-- Gets all the reachable states in the automaton


allReachableStates : Automata.AFD -> List State.State
allReachableStates afd =
    allReachableStatesFromState afd afd.initialState



-- Gets all the reachable states recursively from a state


allReachableStatesFromState : Automata.AFD -> State.State -> List State.State
allReachableStatesFromState afd origin =
    allReachableStatesHelper afd [] [ origin ] []



-- Helper function to get reachable states. Receives an AFD, a list of already
-- known reachable states, a list of untested states and a a list of already
-- tested states


allReachableStatesHelper :
    Automata.AFD
    -> List State.State
    -> List State.State
    -> List State.State
    -> List State.State
allReachableStatesHelper afd reachable untested tested =
    case List.head untested of
        Nothing ->
            reachable

        Just origin ->
            if List.member origin tested then
                allReachableStatesHelper afd reachable (List.drop 1 untested) tested

            else
                let
                    newReachable =
                        reachableStates afd origin
                            |> List.filter
                                (\state -> not (List.member state reachable))

                    newUntested =
                        List.append (List.drop 1 untested) newReachable

                    newTested =
                        origin :: tested
                in
                allReachableStatesHelper afd
                    (reachable ++ newReachable)
                    newUntested
                    newTested



-- ALIVE STATES
-- Gets all the states that can reach a final state


allAliveStates : Automata.AFD -> List State.State
allAliveStates afd =
    let
        isFinal state =
            List.member state afd.finalStates
    in
    List.filter
        (\state -> List.any isFinal (allReachableStatesFromState afd state))
        afd.states



-- CLASSES
-- TODO
