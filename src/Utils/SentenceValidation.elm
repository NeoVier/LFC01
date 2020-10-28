module Utils.SentenceValidation exposing (..)

import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Utils.Utils as Utils


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
                Err "Não implementado"

    else
        Err "Existem símbolos inválidos"


validateSentenceAFD : Automata.AFD -> List Alphabet.Symbol -> Result String Bool
validateSentenceAFD afd symbols =
    if validateSentenceAFDFromState afd.initialState afd symbols then
        Ok True

    else
        Err "Sentença inválida"


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
