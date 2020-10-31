{-
   Utils/Utils.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains various utilitary and helper functions
-}


module Utils.Utils exposing (..)

import Html exposing (a)
import List exposing (concatMap, filter, foldr, map, member, sortBy, sortWith)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.Grammars as Grammars
import Models.Models as Models
import Models.State as State
import Models.Transition as Transition



-- Get all transitions that come out of a state from an AFD


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



-- Get all transitions that come out of a state from an AFND


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



-- Given a transition with multiple conditions, return a list of transitions
-- with one condition each


getFlatTransitionDeterministic :
    Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
getFlatTransitionDeterministic transition =
    map
        (\condition -> { transition | conditions = [ condition ] })
        transition.conditions



-- Given a transition with multiple conditions, return a list of transitions
-- with one condition each


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



-- Get the flat transitions (one condition each) that come out of a state


getFlatOutTransitionsDeterministic :
    Automata.AFD
    -> State.State
    -> List Transition.DeterministicTransition
getFlatOutTransitionsDeterministic afd state =
    getOutTransitionsDeterministic afd state
        |> concatMap getFlatTransitionDeterministic



-- Get the flat transitions (one condition each) that come out of a state


getFlatOutTransitionsNonDeterministic :
    Automata.AFND
    -> State.State
    -> List Transition.NonDeterministicTransition
getFlatOutTransitionsNonDeterministic afnd state =
    getOutTransitionsNonDeterministic afnd state
        |> concatMap getFlatTransitionNonDeterministic



-- Function to compare two transitions so we can sort them


compareTransitionsDeterministic :
    Transition.DeterministicTransition
    -> Transition.DeterministicTransition
    -> Order
compareTransitionsDeterministic a b =
    compare a.conditions b.conditions



-- Function to compare two transitions so we can sort them


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



-- Swaps the first and last elements of a list


swapFirstAndLast : List a -> List a
swapFirstAndLast xs =
    case xs of
        y :: ys ->
            ys ++ [ y ]

        otherwise ->
            []



-- Sorts a list of deterministic transitions


sortTransitionsDeterministic :
    List Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
sortTransitionsDeterministic =
    sortWith compareTransitionsDeterministic



-- Sorts a list of non deterministic transitions


sortTransitionsNonDeterministic :
    List Transition.NonDeterministicTransition
    -> List Transition.NonDeterministicTransition
sortTransitionsNonDeterministic =
    sortWith compareTransitionsNonDeterministic



-- Returns the element at index


elementAt : Int -> List a -> Maybe a
elementAt idx list =
    List.drop idx list |> List.head


last : List a -> Maybe a
last list =
    List.drop (List.length list - 1) list
        |> List.head



-- Given a predicate, return Just a or Nothing


filterMaybe : (a -> Bool) -> a -> Maybe a
filterMaybe f x =
    if f x then
        Just x

    else
        Nothing



-- Invert a List (Maybe a) to a Maybe (List a)


listOfMaybesToMaybeList : List (Maybe a) -> Maybe (List a)
listOfMaybesToMaybeList =
    List.foldr (Maybe.map2 (::)) (Just [])



-- Get all the epsilon transitions from an automaton


getEpsilonTransitions :
    Automata.Automaton
    -> List Transition.NonDeterministicTransition
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



-- Given a list of states, create a new state that contains them


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
                |> List.sort
                |> String.join ", "
                |> State.Valid



-- Given a state, return the list of states that form the state


stateToListOfStates : State.State -> List State.State
stateToListOfStates state =
    case state of
        State.Dead ->
            [ State.Dead ]

        State.Valid label ->
            String.split ", " label |> List.map State.Valid



-- Gets the epsilon star for a state


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
                                True
                    )

        outEpsilonStates =
            List.concatMap
                (\transition -> transition.nextStates)
                outEpsilonTransitions
                |> List.filter (\epsilonState -> epsilonState /= State.Dead)
    in
    state
        :: outEpsilonStates



-- Returns all the possible subsequences of a list


subsequences : List a -> List (List a)
subsequences l =
    case l of
        [] ->
            []

        x :: xs ->
            let
                f ys r =
                    ys :: (x :: ys) :: r
            in
            [ x ] :: List.foldr f [] (subsequences xs)



-- Given a list of transitions and a condition, get all of the transitions that
-- have the same condition that was passed in


transitionsWithSameCondition :
    List Transition.NonDeterministicTransition
    -> Transition.NonDeterministicConditions
    -> List Transition.NonDeterministicTransition
transitionsWithSameCondition transitions c =
    List.filter
        (\transition ->
            case transition.conditions of
                Transition.WithEpsilon symbols ->
                    case c of
                        Transition.WithEpsilon _ ->
                            True

                        Transition.NoEpsilon _ ->
                            False

                Transition.NoEpsilon symbols ->
                    case c of
                        Transition.WithEpsilon _ ->
                            False

                        Transition.NoEpsilon cSymbols ->
                            symbols == cSymbols
        )
        transitions



-- Group a list of transitions by their conditions


groupByConditions :
    List Transition.NonDeterministicTransition
    -> List (List Transition.NonDeterministicTransition)
groupByConditions transitions =
    let
        allConditions =
            List.foldr
                (\transition conditions ->
                    case transition.conditions of
                        Transition.NoEpsilon _ ->
                            if List.member transition.conditions conditions then
                                conditions

                            else
                                transition.conditions :: conditions

                        Transition.WithEpsilon symbols ->
                            if
                                List.any
                                    (\condition ->
                                        case condition of
                                            Transition.WithEpsilon _ ->
                                                True

                                            Transition.NoEpsilon _ ->
                                                False
                                    )
                                    conditions
                            then
                                List.map
                                    (\condition ->
                                        case condition of
                                            Transition.WithEpsilon cSymbols ->
                                                Transition.WithEpsilon
                                                    (cSymbols ++ symbols)

                                            Transition.NoEpsilon _ ->
                                                condition
                                    )
                                    conditions

                            else
                                transition.conditions :: conditions
                )
                []
                transitions
    in
    List.map (transitionsWithSameCondition transitions) allConditions



-- Given a list of transitions that all have the same conditions, join them
-- into a single transition


joinTransitionsWithSameCondition :
    List Transition.NonDeterministicTransition
    -> Transition.NonDeterministicTransition
joinTransitionsWithSameCondition transitions =
    case transitions of
        [] ->
            { prevState = State.Dead
            , nextStates = [ State.Dead ]
            , conditions = Transition.NoEpsilon []
            }

        first :: rest ->
            List.foldr
                (\transition acc ->
                    { acc
                        | conditions =
                            joinConditions acc.conditions transition.conditions
                        , nextStates =
                            acc.nextStates
                                ++ List.filter
                                    (\s -> not (List.member s acc.nextStates))
                                    transition.nextStates
                    }
                )
                first
                rest



-- Given two transitions, join them into a single one


joinConditions :
    Transition.NonDeterministicConditions
    -> Transition.NonDeterministicConditions
    -> Transition.NonDeterministicConditions
joinConditions c1 c2 =
    let
        hasEpsilon =
            case c1 of
                Transition.NoEpsilon _ ->
                    case c2 of
                        Transition.NoEpsilon _ ->
                            False

                        Transition.WithEpsilon _ ->
                            True

                Transition.WithEpsilon _ ->
                    True

        c1Symbols =
            case c1 of
                Transition.NoEpsilon symbols ->
                    symbols

                Transition.WithEpsilon symbols ->
                    symbols

        c2Symbols =
            case c2 of
                Transition.NoEpsilon symbols ->
                    symbols

                Transition.WithEpsilon symbols ->
                    symbols

        newSymbols =
            c1Symbols
                ++ List.filter (\s -> not (List.member s c1Symbols)) c2Symbols
    in
    if hasEpsilon then
        Transition.WithEpsilon newSymbols

    else
        Transition.NoEpsilon newSymbols



-- Converts a String to a Char. If the length of the String != 1, return Nothing


stringToChar : String -> Maybe Char
stringToChar =
    String.toList >> List.head



-- Adds a prefix to the name of a state (if it's a valid state)


addPrefix : String -> State.State -> State.State
addPrefix prefix state =
    case state of
        State.Dead ->
            State.Dead

        State.Valid label ->
            State.Valid (prefix ++ label)



-- Joins two deterministic alphabets


joinDeterministicAlphabets :
    Alphabet.DeterministicAlphabet
    -> Alphabet.DeterministicAlphabet
    -> Alphabet.DeterministicAlphabet
joinDeterministicAlphabets alph1 alph2 =
    List.filter (\s -> not (List.member s alph1)) alph2 ++ alph1



-- Determines whether the first two automata in the list are AFDs


firstTwoAreAFDs : List Models.General -> Bool
firstTwoAreAFDs =
    isJust << getFirstTwoAsAFDs



-- Gets the first two automata of the list as AFDs


getFirstTwoAsAFDs : List Models.General -> Maybe ( Automata.AFD, Automata.AFD )
getFirstTwoAsAFDs generals =
    case List.head generals of
        Just (Models.Automaton (Automata.FiniteDeterministic afd1)) ->
            case List.head (List.drop 1 generals) of
                Just (Models.Automaton (Automata.FiniteDeterministic afd2)) ->
                    Just ( afd1, afd2 )

                otherwise ->
                    Nothing

        otherwise ->
            Nothing



-- Converts a Maybe a to Bool


isJust : Maybe a -> Bool
isJust x =
    case x of
        Just _ ->
            True

        Nothing ->
            False



-- Removes duplicates from list


removeDuplicates : List a -> List a
removeDuplicates =
    let
        rdHelper seen rest =
            case List.head rest of
                Nothing ->
                    seen

                Just h ->
                    if List.member h seen then
                        rdHelper seen (List.drop 1 rest)

                    else
                        rdHelper (seen ++ [ h ]) (List.drop 1 rest)
    in
    rdHelper []



-- Returns the index of an element in a list


indexOf : a -> List a -> Int
indexOf elem xs =
    case List.head xs of
        Nothing ->
            0

        Just x ->
            if x == elem then
                0

            else
                1 + indexOf elem (List.drop 1 xs)



-- Replaces an element by another element. If the original element isn't in the
-- list, the new element is appended to the list


replaceBy : a -> a -> List a -> List a
replaceBy original new xs =
    let
        idx =
            indexOf original xs
    in
    List.take idx xs ++ (new :: List.drop (idx + 1) xs)


maybeMaybeToMaybe : Maybe (Maybe a) -> Maybe a
maybeMaybeToMaybe x =
    case x of
        Nothing ->
            Nothing

        Just Nothing ->
            Nothing

        Just y ->
            y



-- Assumes all of the fromSymbols are equal


groupGrammarProductions :
    List Grammars.Production
    -> Maybe Grammars.Production
groupGrammarProductions l =
    Maybe.map
        (\h ->
            { fromSymbol = h.fromSymbol
            , productions = List.concatMap .productions l
            }
        )
        (List.head l)


productionsWithSameFromSymbol :
    Grammars.Grammar
    -> Grammars.NonTerminalSymbol
    -> List Grammars.Production
productionsWithSameFromSymbol gr symbol =
    List.filter (\production -> production.fromSymbol == symbol) gr.productions


joinGrammarProductions : Grammars.Grammar -> Grammars.Grammar
joinGrammarProductions gr =
    let
        newProductions =
            List.filterMap
                (\nonTerminal ->
                    productionsWithSameFromSymbol gr nonTerminal
                        |> groupGrammarProductions
                )
                gr.nonTerminals
    in
    { gr | productions = newProductions }
