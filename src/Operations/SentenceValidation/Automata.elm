{-
   Operations/SentenceValidation/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to validate sentences
-}


module Operations.SentenceValidation.Automata exposing (validateSentence)

import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Operations.Conversion.Automata as CAutomata
import Operations.SentenceValidation.Common exposing (..)
import Utils.Utils as Utils



{- General sentence validation function, routes to the appropriate automaton
   and checks if the sentence uses only symbols from the alphabet
-}


validateSentence : Automata.Automaton -> String -> Result String Bool
validateSentence automaton sentence =
    let
        sentenceAsSymbols =
            String.toList sentence

        alphabet =
            case automaton of
                Automata.FiniteDeterministic afd ->
                    afd.alphabet

                Automata.FiniteNonDeterministic afnd ->
                    case afnd.alphabet of
                        Alphabet.NDA alph _ ->
                            alph
    in
    if allValidSymbols alphabet sentence then
        case automaton of
            Automata.FiniteDeterministic afd ->
                validateSentenceAFD afd sentenceAsSymbols

            Automata.FiniteNonDeterministic afnd ->
                validateSentenceAFND afnd sentenceAsSymbols

    else
        Err "Existem símbolos inválidos"



{- Validate a sentence using an AFD -}


validateSentenceAFD :
    Automata.AFD
    -> List Char
    -> Result String Bool
validateSentenceAFD afd sentence =
    if validateSentenceAFDFromState afd.initialState afd sentence then
        Ok True

    else
        Err "Sentença inválida"



{- Validate a sentence using an AFD starting from a state -}


validateSentenceAFDFromState :
    State.State
    -> Automata.AFD
    -> List Char
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
                                List.member currSymbol
                                    (List.concatMap
                                        symbolItems
                                        transition.conditions
                                    )
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



{- Validate a sentence using an AFND -}


validateSentenceAFND :
    Automata.AFND
    -> List Char
    -> Result String Bool
validateSentenceAFND afnd =
    validateSentenceAFD (CAutomata.afndToAfd afnd)
