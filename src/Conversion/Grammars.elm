{-
   Conversion/Grammars.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to convert Grammars
-}


module Conversion.Grammars exposing (grToAfd)

import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.Grammars as Grammars
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils


grToAfd : Grammars.Grammar -> Automata.AFND
grToAfd gr =
    let
        finalStates =
            [ newFinalState ]

        alphabet : Alphabet.NonDeterministicAlphabet
        alphabet =
            Alphabet.NDA gr.terminals Alphabet.Epsilon

        states : List State.State
        states =
            List.map symbolToState gr.nonTerminals
                ++ finalStates

        initialState =
            symbolToState gr.initialSymbol

        transitions =
            List.concatMap productionToTransitions gr.productions
    in
    { states = states
    , initialState = initialState
    , finalStates = finalStates
    , alphabet = alphabet
    , transitions = transitions
    }


newFinalState : State.State
newFinalState =
    State.Valid "Final"


symbolToState : Grammars.NonTerminalSymbol -> State.State
symbolToState =
    State.Valid


productionToTransitions :
    Grammars.Production
    -> List Transition.NonDeterministicTransition
productionToTransitions production =
    List.map
        (\body ->
            let
                nextState =
                    case body.toSymbol of
                        Nothing ->
                            newFinalState

                        Just symbol ->
                            symbolToState symbol
            in
            { prevState = symbolToState production.fromSymbol
            , nextStates = [ nextState ]
            , conditions = Transition.NoEpsilon [ body.consumed ]
            }
        )
        production.productions
        |> Utils.groupByConditions
        |> List.map Utils.joinTransitionsWithSameCondition
