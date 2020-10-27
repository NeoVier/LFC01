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


getOutTransitionsNonDeterministic :
    Automata.AFND
    -> State.State
    -> List Transition.NonDeterministicTransition
getOutTransitionsNonDeterministic afnd prevState =
    let
        existing =
            filter (\transition -> transition.prevState == prevState)
                afnd.transitions

        existingSymbols =
            concatMap
                (\transition ->
                    case transition.conditions of
                        Transition.NoEpsilon symbols ->
                            symbols

                        Transition.WithEpsilon symbols ->
                            symbols
                )
                existing

        missingSymbols =
            case afnd.alphabet of
                Alphabet.NDA alph epsilon ->
                    filter (\symbol -> not (member symbol existingSymbols))
                        alph

        hasEpsilon =
            List.any
                (\transition ->
                    case transition.conditions of
                        Transition.NoEpsilon _ ->
                            False

                        Transition.WithEpsilon _ ->
                            True
                )
                existing
    in
    Transition.NonDeterministicTransition prevState
        [ State.Dead ]
        (if hasEpsilon then
            Transition.NoEpsilon missingSymbols

         else
            Transition.WithEpsilon missingSymbols
        )
        :: existing


getFlatTransitionDeterministic :
    Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
getFlatTransitionDeterministic transition =
    map
        (\condition -> { transition | conditions = [ condition ] })
        transition.conditions


getFlatTransitionNonDeterministic :
    Transition.NonDeterministicTransition
    -> List Transition.NonDeterministicTransition
getFlatTransitionNonDeterministic transition =
    case transition.conditions of
        Transition.NoEpsilon conditions ->
            map
                (\condition ->
                    { transition | conditions = Transition.NoEpsilon [ condition ] }
                )
                conditions

        Transition.WithEpsilon conditions ->
            map
                (\condition ->
                    { transition | conditions = Transition.WithEpsilon [ condition ] }
                )
                conditions
                ++ [ { transition | conditions = Transition.WithEpsilon [] } ]


getFlatOutTransitionsDeterministic :
    Automata.AFD
    -> State.State
    -> List Transition.DeterministicTransition
getFlatOutTransitionsDeterministic afd state =
    getOutTransitionsDeterministic afd state
        |> concatMap getFlatTransitionDeterministic


getFlatOutTransitionsNonDeterministic :
    Automata.AFND
    -> State.State
    -> List Transition.NonDeterministicTransition
getFlatOutTransitionsNonDeterministic afnd state =
    getOutTransitionsNonDeterministic afnd state
        |> concatMap getFlatTransitionNonDeterministic


compareTransitionsDeterministic :
    Transition.DeterministicTransition
    -> Transition.DeterministicTransition
    -> Order
compareTransitionsDeterministic a b =
    compare a.conditions b.conditions


compareTransitionsNonDeterministic :
    Transition.NonDeterministicTransition
    -> Transition.NonDeterministicTransition
    -> Order
compareTransitionsNonDeterministic a b =
    let
        getSymbols x =
            case x of
                Transition.NoEpsilon symbols ->
                    symbols

                Transition.WithEpsilon symbols ->
                    symbols
    in
    compare (getSymbols a.conditions) (getSymbols b.conditions)


swapFirstAndLast : List a -> List a
swapFirstAndLast xs =
    case xs of
        y :: ys ->
            ys ++ [ y ]

        otherwise ->
            []


sortTransitionsDeterministic :
    List Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
sortTransitionsDeterministic =
    sortWith compareTransitionsDeterministic


sortTransitionsNonDeterministic :
    List Transition.NonDeterministicTransition
    -> List Transition.NonDeterministicTransition
sortTransitionsNonDeterministic =
    sortWith compareTransitionsNonDeterministic


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


listOfStatesToState : List State.State -> State.State
listOfStatesToState states =
    case states of
        [ State.Dead ] ->
            State.Dead

        otherwise ->
            List.map
                (\state ->
                    case state of
                        State.Dead ->
                            ""

                        State.Valid label ->
                            label
                )
                states
                |> String.join ", "
                |> State.Valid


stateToListOfStates : State.State -> List State.State
stateToListOfStates state =
    case state of
        State.Dead ->
            [ State.Dead ]

        State.Valid label ->
            String.split ", " label |> List.map State.Valid


getEpsilonStar : Automata.AFND -> State.State -> List State.State
getEpsilonStar afnd state =
    let
        outEpsilonTransitions =
            getOutTransitionsNonDeterministic afnd state
                |> filter
                    (\transition ->
                        case transition.conditions of
                            Transition.NoEpsilon dc ->
                                False

                            Transition.WithEpsilon ndc ->
                                ndc /= []
                    )

        outEpsilonStates =
            List.concatMap
                (\transition -> transition.nextStates)
                outEpsilonTransitions
    in
    state :: outEpsilonStates
