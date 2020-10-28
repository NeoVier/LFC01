{-
   Conversion/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to convert between Automata
-}


module Conversion.Automata exposing (afdToGr, afndToAfd)

import Conversion.Alphabet as CAlphabet
import Conversion.Transition as CTransition
import Models.Automata as Automata
import Models.Grammars as Grammars
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils exposing (stateToListOfStates)



-- AFND to AFD
-- Converts an AFND to AFD


afndToAfd : Automata.AFND -> Automata.AFD
afndToAfd afnd =
    let
        newStates =
            Utils.subsequences afnd.states
                |> List.sortBy List.length
                |> List.map Utils.listOfStatesToState
                |> List.filter (\state -> not (List.member state afnd.states))

        -- Add Epsilon star
        newStatesTransitions =
            List.concatMap
                (\complexState ->
                    List.concatMap
                        (Utils.getEpsilonStar afnd)
                        (Utils.stateToListOfStates complexState)
                        |> List.concatMap
                            (\state ->
                                Utils.getOutTransitionsNonDeterministic afnd
                                    state
                            )
                        |> List.filter
                            (\transition ->
                                transition.nextStates /= [ State.Dead ]
                            )
                        |> Utils.groupByConditions
                        |> List.map
                            (\x ->
                                let
                                    newTransition =
                                        Utils.joinTransitionsWithSameCondition x
                                in
                                { newTransition | prevState = complexState }
                            )
                )
                newStates

        newTransitions =
            List.map CTransition.nonDeterministicToDeterministic
                (afnd.transitions ++ newStatesTransitions)
                |> Utils.sortTransitionsDeterministic

        newFinalStates =
            List.filter
                (\state ->
                    Utils.stateToListOfStates state
                        |> List.any (\st -> List.member st afnd.finalStates)
                )
                newStates

        newInitialState =
            Utils.getEpsilonStar afnd afnd.initialState
                |> Utils.listOfStatesToState
    in
    { states = List.append afnd.states newStates
    , initialState = newInitialState
    , finalStates = List.append afnd.finalStates newFinalStates
    , alphabet = CAlphabet.nonDeterministicToDeterministic afnd.alphabet
    , transitions = newTransitions
    }



-- AFD TO GR
-- Converts and AFD to a GR


afdToGr : Automata.AFD -> Grammars.Grammar
afdToGr afd =
    let
        nonTerminals =
            List.map stateToSymbol afd.states
                |> Utils.listOfMaybesToMaybeList
                |> Maybe.withDefault []

        terminals =
            afd.alphabet

        initialSymbol =
            stateToSymbol afd.initialState
                |> Maybe.withDefault 'S'
    in
    { nonTerminals = nonTerminals
    , terminals = terminals
    , productions =
        List.filterMap
            (\transition ->
                transitionToProduction transition afd.finalStates
            )
            afd.transitions
    , initialSymbol = initialSymbol
    }
        |> Utils.joinGrammarProductions



-- Convert a State to a Maybe Symbol


stateToSymbol : State.State -> Maybe Grammars.TerminalSymbol
stateToSymbol state =
    case state of
        State.Dead ->
            Nothing

        State.Valid label ->
            String.toList label
                |> List.head



-- Converts a Transition into a Maybe Grammars.Production


transitionToProduction :
    Transition.DeterministicTransition
    -> List State.State
    -> Maybe Grammars.Production
transitionToProduction transition finalStates =
    case transition.nextState of
        State.Dead ->
            Nothing

        State.Valid _ ->
            let
                toSymbol =
                    stateToSymbol transition.nextState

                isFinal =
                    List.member transition.nextState finalStates

                productions =
                    List.concatMap
                        (\condition ->
                            if isFinal then
                                [ { consumed = condition, toSymbol = toSymbol }
                                , { consumed = condition, toSymbol = Nothing }
                                ]

                            else
                                [ { consumed = condition, toSymbol = toSymbol } ]
                        )
                        transition.conditions
            in
            stateToSymbol transition.prevState
                |> Maybe.map
                    (\fromSymbol ->
                        { fromSymbol = fromSymbol
                        , productions = productions
                        }
                    )
