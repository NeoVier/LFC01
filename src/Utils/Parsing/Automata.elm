module Utils.Parsing.Automata exposing (..)

import Array
import List exposing (head, map)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils


generateStates : Int -> List State.State
generateStates n =
    map
        (\x -> State.Valid (String.fromInt x))
        (List.range 0 (n - 1))


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


parseFiniteDeterministic : String -> Maybe Automata.AFD
parseFiniteDeterministic text =
    let
        lines =
            String.split "\n" text |> Array.fromList

        numberOfStates =
            case Array.get 0 lines of
                Nothing ->
                    Just 0

                Just nStr ->
                    String.toInt nStr

        states : Maybe (List State.State)
        states =
            Maybe.map generateStates numberOfStates

        initialState : Maybe State.State
        initialState =
            Array.get 1 lines
                |> (\x -> getStateWithMaybes x states)

        finalStates : Maybe (List State.State)
        finalStates =
            Maybe.map (String.split ",") (Array.get 2 lines)
                |> Maybe.map (map State.Valid)

        alphabet : Maybe Alphabet.Alphabet
        alphabet =
            Maybe.map
                (\line ->
                    String.split "," line
                        |> Alphabet.Alphabet
                )
                (Array.get 3 lines)

        readTransition : String -> Maybe Transition.DeterministicTransition
        readTransition line =
            let
                items =
                    String.split "," line |> Array.fromList

                prevStateIndex =
                    Array.get 0 items

                prevState =
                    getStateWithMaybes prevStateIndex states

                nextStateIndex =
                    Array.get 2 items

                nextState =
                    getStateWithMaybes nextStateIndex states

                -- TODO Make this better
                symbol : Maybe Alphabet.Symbol
                symbol =
                    case Array.get 1 items of
                        Nothing ->
                            Nothing

                        Just strSymbol ->
                            case alphabet of
                                Nothing ->
                                    Nothing

                                Just alph ->
                                    Utils.filterMaybe
                                        (\x ->
                                            List.member x alph.symbols
                                        )
                                        strSymbol
            in
            Maybe.map3 Transition.DeterministicTransition
                prevState
                nextState
                (Maybe.map (\x -> [ x ]) symbol)

        transitions : Maybe (List Transition.DeterministicTransition)
        transitions =
            List.map readTransition (List.drop 4 (Array.toList lines))
                |> Utils.listOfMaybesToMaybeList
    in
    Maybe.map5 Automata.AFD states initialState finalStates alphabet transitions
