module Utils.Utils exposing (..)

import Html exposing (a)
import List exposing (concatMap, filter, foldr, map, member, sortWith)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition


getOutTransitionsDeterministic :
    Automata.AFD
    -> State.State
    -> List Transition.DeterministicTransition
getOutTransitionsDeterministic afd prevState =
    let
        existing =
            filter (\transition -> transition.prevState == prevState)
                afd.transitions

        existingSymbols =
            concatMap (\transition -> transition.conditions) existing

        missingSymbols =
            filter (\symbol -> not (member symbol existingSymbols))
                afd.alphabet
    in
    Transition.DeterministicTransition prevState State.Dead missingSymbols
        :: existing


getFlatTransitionDeterministic :
    Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
getFlatTransitionDeterministic transition =
    map
        (\condition -> { transition | conditions = [ condition ] })
        transition.conditions


getFlatOutTransitionsDeterministic :
    Automata.AFD
    -> State.State
    -> List Transition.DeterministicTransition
getFlatOutTransitionsDeterministic afd state =
    getOutTransitionsDeterministic afd state
        |> concatMap getFlatTransitionDeterministic


compareTransitions :
    Transition.DeterministicTransition
    -> Transition.DeterministicTransition
    -> Order
compareTransitions a b =
    compare a.conditions b.conditions


sortTransitionsDeterministic :
    List Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
sortTransitionsDeterministic =
    sortWith compareTransitions


elementAt : Int -> List a -> Maybe a
elementAt idx list =
    List.drop idx list |> List.head


filterMaybe : (a -> Bool) -> a -> Maybe a
filterMaybe f x =
    if f x then
        Just x

    else
        Nothing


listOfMaybesToMaybeList : List (Maybe a) -> Maybe (List a)
listOfMaybesToMaybeList =
    List.foldr (Maybe.map2 (::)) (Just [])


getEpsilonTransitions : Automata.Automaton -> List Transition.NonDeterministicTransition
getEpsilonTransitions automaton =
    case automaton of
        Automata.FiniteDeterministic afd ->
            []

        Automata.FiniteNonDeterministic afnd ->
            filter
                (\transition ->
                    case transition.conditions of
                        Transition.NoEpsilon _ ->
                            False

                        Transition.WithEpsilon _ ->
                            True
                )
                afnd.transitions
