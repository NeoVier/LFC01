module Parsing.Automata exposing (..)

import Array
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils


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


generateStates : Int -> List State.State
generateStates n =
    List.map
        (\x -> State.Valid (String.fromInt x))
        (List.range 0 (n - 1))


type CommonItems
    = CommonItems (List State.State) State.State (List State.State) (List Alphabet.Symbol)


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

                transitions : Maybe (List Transition.DeterministicTransition)
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
