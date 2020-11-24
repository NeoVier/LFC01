{-
   Saving/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to save Automata
-}


module Saving.Automata exposing
    ( deterministicAutomatonToString
    , nonDeterministicAutomatonToString
    )

import Dict as Dict exposing (Dict)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils



{- Transform an AFD into a string -}


deterministicAutomatonToString : Automata.AFD -> String
deterministicAutomatonToString afd =
    let
        relabeledAfd =
            labelStatesAsIntsDeterministic afd

        stateNumber =
            List.length afd.states |> String.fromInt

        initialState =
            stateToString afd.initialState

        finalStates =
            List.map stateToString relabeledAfd.finalStates
                |> String.join ","

        alphabet =
            List.map symbolToString afd.alphabet
                |> String.join ","

        transitions =
            List.concatMap
                (Utils.getFlatTransitionDeterministic
                    >> List.filterMap
                        (\transition ->
                            case transition.nextState of
                                State.Dead ->
                                    Nothing

                                _ ->
                                    flatDeterministicTransitionToString transition
                                        |> Just
                        )
                )
                relabeledAfd.transitions
                |> List.sort
                |> String.join "\n"
    in
    String.join "\n"
        [ stateNumber
        , initialState
        , finalStates
        , alphabet
        , transitions
        ]



{- Transform an AFD into a string -}


nonDeterministicAutomatonToString : Automata.AFND -> String
nonDeterministicAutomatonToString afnd =
    let
        relabeledAfnd =
            labelStatesAsIntsNonDeterministic afnd

        stateNumber =
            List.length afnd.states |> String.fromInt

        initialState =
            stateToString afnd.initialState

        finalStates =
            List.map stateToString relabeledAfnd.finalStates
                |> String.join ","

        hasEpsilon =
            List.any
                (\transition ->
                    case transition.conditions of
                        Transition.WithEpsilon _ ->
                            True

                        Transition.NoEpsilon _ ->
                            False
                )
                afnd.transitions

        symbols =
            case afnd.alphabet of
                Alphabet.NDA s _ ->
                    if hasEpsilon then
                        s ++ [ Alphabet.Single '&' ]

                    else
                        s

        alphabet =
            List.map symbolToString symbols
                |> String.join ","

        transitions =
            List.concatMap
                (Utils.getFlatTransitionNonDeterministic
                    >> List.filterMap
                        (\transition ->
                            case transition.nextStates of
                                [ State.Dead ] ->
                                    Nothing

                                _ ->
                                    flatNonDeterministicTransitionToString
                                        transition
                                        |> Just
                        )
                )
                relabeledAfnd.transitions
                |> List.sort
                |> String.join "\n"
    in
    String.join "\n"
        [ stateNumber
        , initialState
        , finalStates
        , alphabet
        , transitions
        ]



{- Transform a state into a string -}


stateToString : State.State -> String
stateToString s =
    case s of
        State.Dead ->
            ""

        State.Valid label ->
            label



{- Transform a symbol into a string -}


symbolToString : Alphabet.Symbol -> String
symbolToString s =
    case s of
        Alphabet.Single c ->
            String.fromChar c

        Alphabet.Group g ->
            "["
                ++ String.join ""
                    (List.map
                        (\( c1, c2 ) ->
                            String.fromList [ c1, '-', c2 ]
                        )
                        g
                    )
                ++ "]"



{- Transform a flat deterministic transition into a string -}


flatDeterministicTransitionToString :
    Transition.DeterministicTransition
    -> String
flatDeterministicTransitionToString t =
    case List.head t.conditions of
        Nothing ->
            ""

        Just symbol ->
            String.join ","
                [ stateToString t.prevState
                , symbolToString symbol
                , stateToString t.nextState
                ]



{- Transform a flat non deterministic transition into a string -}


flatNonDeterministicTransitionToString :
    Transition.NonDeterministicTransition
    -> String
flatNonDeterministicTransitionToString t =
    let
        ( hasEpsilon, conditions ) =
            case t.conditions of
                Transition.NoEpsilon c ->
                    ( False, c )

                Transition.WithEpsilon c ->
                    ( True, c )
    in
    case List.head conditions of
        Nothing ->
            if not hasEpsilon then
                ""

            else
                String.join ","
                    [ stateToString t.prevState
                    , "&"
                    , String.join "-" (List.map stateToString t.nextStates)
                    ]

        Just symbol ->
            String.join ","
                [ stateToString t.prevState
                , symbolToString symbol
                , String.join "-" (List.map stateToString t.nextStates)
                ]



{- Rename a state -}


renameState : State.State -> String -> State.State
renameState state newName =
    case state of
        State.Dead ->
            State.Dead

        State.Valid _ ->
            State.Valid newName



{- Rename a state based on a dict -}


findAndRenameState : State.State -> Dict String String -> State.State
findAndRenameState state dict =
    case Dict.get (stateToString state) dict of
        Nothing ->
            State.Dead

        Just newName ->
            renameState state newName



{- Transform states into numbers -}


labelStatesAsIntsDeterministic : Automata.AFD -> Automata.AFD
labelStatesAsIntsDeterministic afd =
    let
        newNames =
            List.map2
                (\prevName idx ->
                    ( stateToString prevName
                    , String.fromInt idx
                    )
                )
                afd.states
                (List.range 0 (List.length afd.states))
                |> Dict.fromList

        initialState =
            State.Valid "0"

        finalStatesLabels =
            List.map stateToString afd.finalStates

        renameTransition transition =
            { transition
                | prevState = findAndRenameState transition.prevState newNames
                , nextState = findAndRenameState transition.nextState newNames
            }
    in
    { afd
        | states =
            initialState
                :: (List.map (State.Valid << String.fromInt) <|
                        List.range 1 (List.length afd.states - 1)
                   )
        , initialState = initialState
        , finalStates =
            Dict.keys newNames
                |> List.filter (\old -> List.member old finalStatesLabels)
                |> List.map State.Valid
        , transitions = List.map renameTransition afd.transitions
    }



{- Transform states into numbers -}


labelStatesAsIntsNonDeterministic : Automata.AFND -> Automata.AFND
labelStatesAsIntsNonDeterministic afnd =
    let
        newNames =
            List.map2
                (\prevName idx ->
                    ( stateToString prevName, String.fromInt idx )
                )
                afnd.states
                (List.range 0 (List.length afnd.states))
                |> Dict.fromList

        initialState =
            State.Valid "0"

        finalStatesLabels =
            List.map stateToString afnd.finalStates

        renameTransition transition =
            { transition
                | prevState = findAndRenameState transition.prevState newNames
                , nextStates =
                    List.map (\state -> findAndRenameState state newNames)
                        transition.nextStates
            }
    in
    { afnd
        | states =
            initialState
                :: (List.map (State.Valid << String.fromInt) <|
                        List.range 1 (List.length afnd.states - 1)
                   )
        , initialState = initialState
        , finalStates =
            Dict.keys newNames
                |> List.filter (\old -> List.member old finalStatesLabels)
                |> List.map State.Valid
        , transitions = List.map renameTransition afnd.transitions
    }
