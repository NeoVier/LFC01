{-
   Operations/SentenceValidation/GLC
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains functions to validate sentences using a GLC
-}


module Operations.SentenceValidation.GLC exposing (validateSentence)

import Models.Grammars exposing (..)
import Operations.GLC exposing (eliminateLeftRecursion, factorGLC)
import Operations.SentenceValidation.Common exposing (..)



{- Validate a sentence using a GLC. Uses LL(1) -}


validateSentence : ContextFreeGrammar -> String -> Result String Bool
validateSentence inputGlc sentence =
    let
        fixedGlc =
            glcToValidate inputGlc
    in
    if allValidSymbols inputGlc.terminals sentence then
        case fixedGlc of
            Nothing ->
                Err "LL(1) não pode ser usado (gramática não pôde ser fatorada ou ter sua recursão à esquerda eliminada)"

            Just glc ->
                Err "TODO"

    else
        Err "Existem símbolos inválidos"



{- Adjust a GLC to validate sentences -}


glcToValidate : ContextFreeGrammar -> Maybe ContextFreeGrammar
glcToValidate glc =
    eliminateLeftRecursion glc |> Result.andThen factorGLC |> Result.toMaybe
