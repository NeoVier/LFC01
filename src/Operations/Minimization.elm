{-
   Operations/Minimization.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to minimize automata
-}


module Operations.Minimization exposing (minimizeAFD)

import Models.Alphabet as Alphabet
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
        |> afdToClassList
        |> afdFromClassList



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
-- Type with necessary definitions for a Class


type alias Class =
    { states : List State.State
    , transitions : List Transition.DeterministicTransition
    , isFinal : Bool
    , isInitial : Bool
    }



-- Given a Class list, retrieve an AFD


afdFromClassList : List Class -> Automata.AFD
afdFromClassList classes =
    let
        states =
            List.indexedMap (\i _ -> State.Valid (String.fromInt i)) classes

        transitions : List Transition.DeterministicTransition
        transitions =
            List.concat <|
                List.indexedMap
                    (\classIndex class ->
                        List.map
                            (\t ->
                                { t
                                    | prevState =
                                        State.Valid (String.fromInt classIndex)
                                    , nextState =
                                        if t.nextState == State.Dead then
                                            State.Dead

                                        else
                                            getStateClassIndex t.nextState
                                                classes
                                                |> String.fromInt
                                                |> State.Valid
                                }
                            )
                            class.transitions
                    )
                    classes

        initialState : State.State
        initialState =
            List.filter .isInitial classes
                |> List.head
                |> Maybe.map (\class -> Utils.indexOf class classes)
                |> Maybe.withDefault 0
                |> String.fromInt
                |> State.Valid

        finalStates : List State.State
        finalStates =
            List.filter .isFinal classes
                |> List.map (\class -> Utils.indexOf class classes)
                |> List.map (String.fromInt >> State.Valid)

        alphabet : Alphabet.DeterministicAlphabet
        alphabet =
            List.concatMap
                (\class -> List.concatMap .conditions class.transitions)
                classes
                |> Utils.removeDuplicates
    in
    { states = states
    , initialState = initialState
    , finalStates = finalStates
    , alphabet = alphabet
    , transitions = transitions
    }



-- Given an automaton, retrieve the optimized list of Classes


afdToClassList : Automata.AFD -> List Class
afdToClassList afd =
    let
        nonFinalStates =
            List.filter (\state -> not <| List.member state afd.finalStates)
                afd.states

        initialClasses =
            [ { states = afd.finalStates
              , transitions = []
              , isFinal = True
              , isInitial = False
              }
            , { states = nonFinalStates
              , transitions = []
              , isFinal = False
              , isInitial = False
              }
            ]

        result =
            recurseClasses initialClasses afd
                |> List.filter
                    (\c ->
                        not (List.isEmpty c.transitions)
                            && not (List.isEmpty c.states)
                    )

        initialClass =
            List.filter (\c -> List.member afd.initialState c.states) result
                |> List.head
    in
    case initialClass of
        Nothing ->
            result

        Just c ->
            Utils.replaceBy c { c | isInitial = True } result



-- Helper function to recurse with classStep


recurseClasses : List Class -> Automata.AFD -> List Class
recurseClasses classes afd =
    let
        result =
            classStep classes afd
    in
    if result == classes then
        classes

    else
        recurseClasses result afd



-- Do a step in the Class assigning algorithm


classStep : List Class -> Automata.AFD -> List Class
classStep classes afd =
    List.foldl (\state classList -> assignClass state classList afd)
        classes
        afd.states



-- Insert a state into a Class list


assignClass : State.State -> List Class -> Automata.AFD -> List Class
assignClass state classes afd =
    let
        transitions =
            Utils.getFlatOutTransitionsDeterministic afd state
                |> Utils.sortTransitionsDeterministic

        sameTransitions =
            List.filter
                (\class ->
                    equivalentTransitionLists transitions
                        class.transitions
                        classes
                )
                classes

        prevClass =
            List.filter (\class -> List.member state class.states) classes
                |> List.head

        isFinal =
            List.member state afd.finalStates
    in
    case List.head sameTransitions of
        Nothing ->
            case prevClass of
                Nothing ->
                    classes
                        ++ [ { states = [ state ]
                             , transitions = transitions
                             , isFinal = isFinal
                             , isInitial = False
                             }
                           ]
                        |> Utils.removeDuplicates

                Just class ->
                    (Utils.replaceBy class
                        { class
                            | states =
                                List.filter (\s -> s /= state) class.states
                        }
                        classes
                        |> List.filter (\c -> not <| List.isEmpty c.states)
                    )
                        ++ [ { states = [ state ]
                             , transitions = transitions
                             , isFinal = isFinal
                             , isInitial = False
                             }
                           ]
                        |> Utils.removeDuplicates

        Just class ->
            Utils.replaceBy class
                { class
                    | states =
                        class.states ++ [ state ] |> Utils.removeDuplicates
                }
                classes
                |> Utils.removeDuplicates



-- Determine if two lists of transitions are equivalent


equivalentTransitionLists :
    List Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
    -> List Class
    -> Bool
equivalentTransitionLists t1s t2s classes =
    List.length t1s
        == List.length t2s
        && (List.map2 (\t1 t2 -> equivalentTransitions t1 t2 classes) t1s t2s
                |> List.all identity
           )



-- Determine if two transitions are equivalent


equivalentTransitions :
    Transition.DeterministicTransition
    -> Transition.DeterministicTransition
    -> List Class
    -> Bool
equivalentTransitions t1 t2 classes =
    let
        toClassMaybe t =
            List.filter (\class -> List.member t.nextState class.states)
                classes
                |> List.head

        fromClassMaybe t =
            List.filter (\class -> List.member t.prevState class.states)
                classes
                |> List.head

        isFinal t =
            case fromClassMaybe t of
                Nothing ->
                    False

                Just class ->
                    class.isFinal
    in
    case toClassMaybe t1 of
        Nothing ->
            False

        Just t1ToClass ->
            case toClassMaybe t2 of
                Nothing ->
                    False

                Just t2ToClass ->
                    t1ToClass
                        == t2ToClass
                        && t1.conditions
                        == t2.conditions
                        && isFinal t1
                        == isFinal t2



-- Get the index of the class to which the state belongs


getStateClassIndex : State.State -> List Class -> Int
getStateClassIndex state classes =
    case classes of
        [] ->
            0

        x :: xs ->
            if List.member state x.states then
                0

            else
                1 + getStateClassIndex state xs
