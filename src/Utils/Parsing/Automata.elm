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


readTransition :
    String
    -> Maybe (List State.State)
    -> Maybe Alphabet.DeterministicAlphabet
    -> Maybe Transition.DeterministicTransition
readTransition line states alphabet =
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

        symbol =
            Maybe.andThen
                (\alph ->
                    Array.get 1 items
                        |> Maybe.andThen
                            (Utils.filterMaybe (\x -> List.member x alph))
                )
                alphabet
    in
    Maybe.map3 Transition.DeterministicTransition
        prevState
        nextState
        (Maybe.map (\x -> [ x ]) symbol)


parseFiniteDeterministic : String -> Maybe Automata.AFD
parseFiniteDeterministic text =
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
                |> Maybe.map (map State.Valid)

        alphabet =
            Maybe.map
                (\line ->
                    String.split "," line
                )
                (Array.get 3 lines)

        transitions =
            List.map (\line -> readTransition line states alphabet)
                (List.drop 4 (Array.toList lines))
                |> Utils.listOfMaybesToMaybeList
    in
    Maybe.map5 Automata.AFD states initialState finalStates alphabet transitions
