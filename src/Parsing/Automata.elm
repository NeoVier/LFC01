{-
   Parsing/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to parse Automata
-}


module Parsing.Automata exposing (parseAFD, parseAFND)

import Models.Alphabet as Alphabet
import Models.Automata as Automata exposing (AFD, AFND, Automaton)
import Models.State as State
import Models.Transition as Transition
import Parser as P exposing ((|.), (|=), Parser)
import Parsing.Common as PC
import Result.Extra as ResultE
import Utils.Utils as Utils



{- Parse an AFD file -}


parseAFD : String -> Maybe AFD
parseAFD content =
    let
        lines =
            String.lines content

        stateAmountLine =
            Utils.elementAt 0 lines
                |> Maybe.withDefault ""

        rStates =
            P.run stateAmount stateAmountLine

        initialStateLine =
            Utils.elementAt 1 lines
                |> Maybe.withDefault ""

        rInitialState =
            P.run state initialStateLine

        finalStatesLine =
            Utils.elementAt 2 lines
                |> Maybe.withDefault ""

        rFinalStates =
            P.run stateList finalStatesLine

        alphabetLine =
            Utils.elementAt 3 lines
                |> Maybe.withDefault ""

        rAlphabet =
            P.run alphabetDeterministic alphabetLine

        transitionsLines =
            List.drop 4 lines

        rTransitions =
            List.map (P.run transitionDeterministic) transitionsLines
                |> ResultE.combine
    in
    Result.map5
        (\s iS fS a t ->
            { states = s
            , initialState = iS
            , finalStates = fS
            , alphabet = a
            , transitions = t
            }
        )
        rStates
        rInitialState
        rFinalStates
        rAlphabet
        rTransitions
        |> Result.toMaybe



{- Parse an AFND file -}


parseAFND : String -> Maybe AFND
parseAFND content =
    let
        lines =
            String.lines content

        stateAmountLine =
            Utils.elementAt 0 lines
                |> Maybe.withDefault ""

        rStates =
            P.run stateAmount stateAmountLine

        initialStateLine =
            Utils.elementAt 1 lines
                |> Maybe.withDefault ""

        rInitialState =
            P.run state initialStateLine

        finalStatesLine =
            Utils.elementAt 2 lines
                |> Maybe.withDefault ""

        rFinalStates =
            P.run stateList finalStatesLine

        alphabetLine =
            Utils.elementAt 3 lines
                |> Maybe.withDefault ""

        rAlphabet =
            P.run alphabetNonDeterministic alphabetLine

        transitionsLines =
            List.drop 4 lines

        rTransitions =
            List.map (P.run transitionNonDeterministic) transitionsLines
                |> ResultE.combine
    in
    Result.map5
        (\s iS fS a t ->
            { states = s
            , initialState = iS
            , finalStates = fS
            , alphabet = a
            , transitions = t
            }
        )
        rStates
        rInitialState
        rFinalStates
        rAlphabet
        rTransitions
        |> Result.toMaybe



{- Get a list of states based on an int -}


stateAmount : Parser (List State.State)
stateAmount =
    P.int |> P.map generateStates



{- Get a state based on an int -}


state : Parser State.State
state =
    P.int |> P.map intToState



{- Get a list of comma-separated states -}


stateList : Parser (List State.State)
stateList =
    P.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = P.spaces
        , item = state
        , trailing = P.Forbidden
        }



{- Parse a deterministic alphabet -}


alphabetDeterministic : Parser Alphabet.DeterministicAlphabet
alphabetDeterministic =
    P.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = P.spaces
        , item = PC.alphabetSymbol
        , trailing = P.Forbidden
        }



{- Parse a non deterministic alphabet -}


alphabetNonDeterministic : Parser Alphabet.NonDeterministicAlphabet
alphabetNonDeterministic =
    P.oneOf
        [ P.sequence
            { start = ""
            , separator = ","
            , end = ""
            , spaces = P.spaces
            , item =
                P.oneOf
                    [ PC.alphabetSymbol
                    , epsilon
                    ]
            , trailing = P.Forbidden
            }
            |> P.map
                (\x ->
                    Alphabet.NDA x Alphabet.Epsilon
                )
        ]
        |> P.map removeEpsilon



{- Removes the '&' symbol from a NonDeterministicAlphabet, because the epsilon
   parser needs to return a value, but we don't need it
-}


removeEpsilon :
    Alphabet.NonDeterministicAlphabet
    -> Alphabet.NonDeterministicAlphabet
removeEpsilon alph =
    case alph of
        Alphabet.NDA symbols Alphabet.Epsilon ->
            Alphabet.NDA
                (List.filter (\s -> s /= Alphabet.Single '&') symbols)
                Alphabet.Epsilon



{- Parse epsilon as '&' -}


epsilon : Parser Alphabet.Symbol
epsilon =
    P.getChompedString (P.chompIf (\c -> c == '&'))
        |> P.andThen PC.createChar
        |> P.map Alphabet.Single



{- Parse a deterministic transition -}


transitionDeterministic : Parser Transition.DeterministicTransition
transitionDeterministic =
    P.succeed
        (\p cs n ->
            { prevState = p
            , nextState = n
            , conditions = [ cs ]
            }
        )
        |= state
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= PC.alphabetSymbol
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= state



{- Parse a non deterministic transition -}


transitionNonDeterministic : Parser Transition.NonDeterministicTransition
transitionNonDeterministic =
    P.oneOf
        [ P.backtrackable <|
            P.succeed
                (\p cs n ->
                    { prevState = p
                    , nextStates = n
                    , conditions = Transition.NoEpsilon [ cs ]
                    }
                )
                |= state
                |. P.spaces
                |. P.symbol ","
                |. P.spaces
                |= PC.alphabetSymbol
                |. P.spaces
                |. P.symbol ","
                |. P.spaces
                |= P.sequence
                    { start = ""
                    , separator = "-"
                    , end = ""
                    , spaces = P.spaces
                    , item = state
                    , trailing = P.Forbidden
                    }
        , P.succeed
            (\p n ->
                { prevState = p
                , nextStates = n
                , conditions = Transition.WithEpsilon []
                }
            )
            |= state
            |. P.spaces
            |. P.symbol ","
            |. P.spaces
            |. epsilon
            |. P.spaces
            |. P.symbol ","
            |. P.spaces
            |= P.sequence
                { start = ""
                , separator = "-"
                , end = ""
                , spaces = P.spaces
                , item = state
                , trailing = P.Forbidden
                }
        ]



{- Helper function to generate states labeled from "0" to "n" -}


generateStates : Int -> List State.State
generateStates n =
    List.map intToState (List.range 0 (n - 1))


intToState : Int -> State.State
intToState =
    String.fromInt >> State.Valid
