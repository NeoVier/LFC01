{-
   Parsing/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to parse Automata
-}


module Parsing.Automata exposing (parseAFD, parseAFND)

import Array
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils



-- Helper function to get a state


getStateWithMaybes :
    Maybe String
    -> Maybe (List State.State)
    -> Maybe State.State
getStateWithMaybes strIndex states =
    Maybe.andThen String.toInt strIndex
        |> Maybe.andThen
            (\x ->
                Maybe.andThen (Utils.elementAt x) states
            )



-- Helper function to generate states labeled from "0" to "n"


generateStates : Int -> List State.State
generateStates n =
    List.map
        (\x -> State.Valid (String.fromInt x))
        (List.range 0 (n - 1))



-- A type that defines the common items between automata, just so we can reuse
-- some stuff


type CommonItems
    = CommonItems (List State.State) State.State (List State.State) (List Alphabet.Symbol)



-- Parse all the common items given in an input


parseCommons : String -> Maybe CommonItems
parseCommons text =
    let
        lines =
            String.split "\n" text |> Array.fromList

        numberOfStates =
            Array.get 0 lines |> Maybe.andThen String.toInt

        states =
            Maybe.map generateStates numberOfStates

        initialState =
            Array.get 1 lines
                |> (\x -> getStateWithMaybes x states)

        finalStates =
            Maybe.map (String.split ",") (Array.get 2 lines)
                |> Maybe.map (List.map State.Valid)

        alphabet =
            Maybe.map (\line -> String.split "," line)
                (Array.get 3 lines)
    in
    Maybe.map4 CommonItems states initialState finalStates alphabet



-- Parse a deterministic transition line


parseDeterministicTransition :
    String
    -> List State.State
    -> Alphabet.DeterministicAlphabet
    -> Maybe Transition.DeterministicTransition
parseDeterministicTransition line states alphabet =
    let
        items =
            String.split "," line |> Array.fromList

        prevStateIndex =
            Array.get 0 items

        prevState =
            Maybe.andThen String.toInt prevStateIndex
                |> Maybe.andThen (\x -> Utils.elementAt x states)

        nextStateIndex =
            Array.get 2 items

        nextState =
            Maybe.andThen String.toInt nextStateIndex
                |> Maybe.andThen (\x -> Utils.elementAt x states)

        symbol =
            Array.get 1 items
                |> Maybe.andThen
                    (Utils.filterMaybe
                        (\x ->
                            List.member x alphabet
                        )
                    )
    in
    Maybe.map3
        Transition.DeterministicTransition
        prevState
        nextState
        (Maybe.map (\x -> [ x ]) symbol)



-- Parse a non deterministic transition line


parseNonDeterministicTransition :
    String
    -> List State.State
    -> List Alphabet.Symbol
    -> Maybe Transition.NonDeterministicTransition
parseNonDeterministicTransition line states symbols =
    let
        items =
            String.split "," line
                |> Array.fromList

        prevStateIndex =
            Array.get 0 items

        prevState =
            Maybe.andThen String.toInt prevStateIndex
                |> Maybe.andThen (\x -> Utils.elementAt x states)

        nextStateIndexes =
            Array.get 2 items |> Maybe.map (String.split "-")

        nextStates : Maybe (List State.State)
        nextStates =
            Maybe.andThen
                (\strIndexes ->
                    List.map
                        (\strIndex ->
                            String.toInt strIndex
                                |> Maybe.andThen (\x -> Utils.elementAt x states)
                        )
                        strIndexes
                        |> Utils.listOfMaybesToMaybeList
                )
                nextStateIndexes

        symbol =
            Array.get 1 items
                |> Maybe.andThen
                    (Utils.filterMaybe
                        (\x ->
                            List.member x symbols
                        )
                    )

        epsilon =
            symbol == Just "&"

        alphabet =
            if epsilon then
                Just (Transition.WithEpsilon [])

            else
                Maybe.map (\s -> Transition.NoEpsilon [ s ]) symbol
    in
    Maybe.map3
        Transition.NonDeterministicTransition
        prevState
        nextStates
        alphabet



-- Parse a Finite Deterministic Automaton


parseAFD : String -> Maybe Automata.AFD
parseAFD text =
    let
        commons =
            parseCommons text
    in
    case commons of
        Nothing ->
            Nothing

        Just (CommonItems states initialState finalStates symbols) ->
            let
                lines =
                    String.lines text
                        |> List.drop 4

                transitions =
                    List.map
                        (\line ->
                            parseDeterministicTransition line states symbols
                        )
                        lines
                        |> Utils.listOfMaybesToMaybeList
            in
            Maybe.map
                (\t ->
                    Automata.AFD states initialState finalStates symbols t
                )
                transitions



-- Parse a Finite Non Deterministic Automaton


parseAFND : String -> Maybe Automata.AFND
parseAFND text =
    let
        commons =
            parseCommons text
    in
    case commons of
        Nothing ->
            Nothing

        Just (CommonItems states initialState finalStates symbols) ->
            let
                lines =
                    String.lines text
                        |> List.drop 4

                transitions =
                    List.map
                        (\line ->
                            parseNonDeterministicTransition line states symbols
                        )
                        lines
                        |> Utils.listOfMaybesToMaybeList

                alphabet =
                    Alphabet.NDA
                        (List.filter
                            (\x ->
                                x /= "&"
                            )
                            symbols
                        )
                        Alphabet.Epsilon
            in
            Maybe.map
                (\t ->
                    Automata.AFND states initialState finalStates alphabet t
                )
                transitions
