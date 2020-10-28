{-
   Operations/SentenceValidation.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to validate sentences
-}


module Operations.SentenceValidation exposing (validateSentence)

import Conversion.Automata as CAutomata
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Utils.Utils as Utils



-- General sentence validation function, routes to the appropriate automaton
-- and checks if the sentence uses only symbols from the alphabet


validateSentence : Automata.Automaton -> String -> Result String Bool
validateSentence automaton sentence =
    let
        symbols =
            case automaton of
                Automata.FiniteDeterministic afd ->
                    afd.alphabet

                Automata.FiniteNonDeterministic afnd ->
                    case afnd.alphabet of
                        Alphabet.NDA alphabet _ ->
                            alphabet

        sentenceAsSymbols =
            String.split "" sentence
    in
    if List.all (\symbol -> List.member symbol symbols) sentenceAsSymbols then
        case automaton of
            Automata.FiniteDeterministic afd ->
                validateSentenceAFD afd sentenceAsSymbols

            Automata.FiniteNonDeterministic afnd ->
                validateSentenceAFND afnd sentenceAsSymbols

    else
        Err "Existem símbolos inválidos"



-- Validate a sentence using an AFD


validateSentenceAFD :
    Automata.AFD
    -> List Alphabet.Symbol
    -> Result String Bool
validateSentenceAFD afd sentence =
    if validateSentenceAFDFromState afd.initialState afd sentence then
        Ok True

    else
        Err "Sentença inválida"



-- Validate a sentence using an AFD starting from a state


validateSentenceAFDFromState :
    State.State
    -> Automata.AFD
    -> List Alphabet.Symbol
    -> Bool
validateSentenceAFDFromState currState afd sentence =
    case sentence of
        [] ->
            List.member currState afd.finalStates

        currSymbol :: rest ->
            let
                outTransitionsWithSymbol =
                    Utils.getOutTransitionsDeterministic afd currState
                        |> List.filter
                            (\transition ->
                                List.member currSymbol transition.conditions
                            )
            in
            case outTransitionsWithSymbol of
                [ transition ] ->
                    case transition.nextState of
                        State.Dead ->
                            False

                        nextState ->
                            validateSentenceAFDFromState nextState
                                afd
                                rest

                otherwise ->
                    False



-- Validate a sentence using an AFND


validateSentenceAFND :
    Automata.AFND
    -> List Alphabet.Symbol
    -> Result String Bool
validateSentenceAFND afnd =
    validateSentenceAFD (CAutomata.afndToAfd afnd)
