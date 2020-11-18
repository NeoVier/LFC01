{-
   Conversion/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to convert between Automata
-}


module Conversion.Automata exposing (afdToGr, afndToAfd)

import Conversion.Alphabet as CAlphabet
import Conversion.Transition as CTransition
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.Grammars as Grammars
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils



-- AFND to AFD
-- Converts an AFND to AFD


afndToAfd : Automata.AFND -> Automata.AFD
afndToAfd afnd =
    let
        -- All combinations of states
        newStates =
            Utils.subsequences afnd.states
                |> List.sortBy List.length
                |> List.map Utils.listOfStatesToState

        newTransitions : List Transition.DeterministicTransition
        newTransitions =
            List.concatMap
                (\complexState ->
                    Utils.stateToListOfStates complexState
                        |> List.concatMap (getTransitions afnd)
                        |> List.map (\t -> { t | prevState = complexState })
                        |> List.filter (.nextState >> (/=) State.Dead)
                        |> Utils.removeDuplicates
                )
                newStates
                |> Utils.removeDuplicates
                |> Utils.sortTransitionsDeterministic
                |> Utils.joinTransitionsDeterministic

        newFinalStates =
            List.filter
                (\state ->
                    Utils.stateToListOfStates state
                        |> List.any (\st -> List.member st afnd.finalStates)
                )
                newStates

        newInitialState =
            Utils.followEpsilonStar afnd afnd.initialState
                |> Utils.listOfStatesToState
    in
    { states = newStates
    , initialState = newInitialState
    , finalStates = List.append afnd.finalStates newFinalStates
    , alphabet = CAlphabet.nonDeterministicToDeterministic afnd.alphabet
    , transitions = newTransitions
    }


getTransitionThrough :
    Automata.AFND
    -> State.State
    -> Alphabet.Symbol
    -> Transition.DeterministicTransition
getTransitionThrough afnd state symbol =
    let
        epsilonStar =
            Utils.followEpsilonStar afnd state

        allStates =
            List.concatMap (Utils.getFlatOutTransitionsNonDeterministic afnd)
                epsilonStar
                |> List.filter
                    (\t ->
                        t.conditions
                            == Transition.NoEpsilon [ symbol ]
                            && t.nextStates
                            /= [ State.Dead ]
                    )
                |> List.concatMap
                    (.nextStates
                        >> List.concatMap (Utils.followEpsilonStar afnd)
                    )
                |> Utils.removeDuplicates
    in
    { prevState = state
    , conditions = [ symbol ]
    , nextState = Utils.listOfStatesToState allStates
    }


getTransitions :
    Automata.AFND
    -> State.State
    -> List Transition.DeterministicTransition
getTransitions afnd origin =
    CAlphabet.nonDeterministicToDeterministic afnd.alphabet
        |> List.map (getTransitionThrough afnd origin)



-- AFD TO GR
-- Converts and AFD to a GR


afdToGr : Automata.AFD -> Grammars.RegularGrammar
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
                |> Maybe.withDefault "S"
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
    , acceptsEmpty = List.member afd.initialState afd.finalStates
    }
        |> Utils.joinGrammarProductions
        |> cleanGR


cleanGR : Grammars.RegularGrammar -> Grammars.RegularGrammar
cleanGR gr =
    let
        validNonTerminals =
            List.filter
                (\nonTerminal ->
                    List.map .fromSymbol gr.productions
                        |> List.member nonTerminal
                )
                gr.nonTerminals

        cleanProduction : Grammars.Production -> Grammars.Production
        cleanProduction prod =
            { prod
                | productions =
                    List.filter
                        (\body ->
                            case body.toSymbol of
                                Nothing ->
                                    True

                                Just s ->
                                    List.member s validNonTerminals
                        )
                        prod.productions
            }
    in
    { gr
        | nonTerminals = validNonTerminals
        , productions =
            List.map cleanProduction gr.productions
    }



-- Convert a State to a Maybe Symbol


stateToSymbol : State.State -> Maybe Grammars.NonTerminalSymbol
stateToSymbol state =
    case state of
        State.Dead ->
            Nothing

        State.Valid label ->
            Just label



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
