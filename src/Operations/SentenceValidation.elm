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
                    List.concatMap symbolItems afd.alphabet

                Automata.FiniteNonDeterministic afnd ->
                    case afnd.alphabet of
                        Alphabet.NDA alphabet _ ->
                            List.concatMap symbolItems alphabet

        sentenceAsSymbols =
            String.toList sentence
    in
    if List.all (\symbol -> List.member symbol symbols) sentenceAsSymbols then
        case automaton of
            Automata.FiniteDeterministic afd ->
                validateSentenceAFD afd sentenceAsSymbols

            Automata.FiniteNonDeterministic afnd ->
                validateSentenceAFND afnd sentenceAsSymbols

    else
        Err "Existem símbolos inválidos"


symbolItems : Alphabet.Symbol -> List Char
symbolItems symbol =
    case symbol of
        Alphabet.Single s ->
            [ s ]

        Alphabet.Group g ->
            List.concatMap innerGroupRange g


innerGroupRange : ( Char, Char ) -> List Char
innerGroupRange group =
    case group of
        ( a, b ) ->
            List.range (Char.toCode a) (Char.toCode b)
                |> List.map Char.fromCode



-- Validate a sentence using an AFD


validateSentenceAFD :
    Automata.AFD
    -> List Char
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



-- Validate a sentence using an AFND


validateSentenceAFND :
    Automata.AFND
    -> List Char
    -> Result String Bool
validateSentenceAFND afnd =
    validateSentenceAFD (CAutomata.afndToAfd afnd)
