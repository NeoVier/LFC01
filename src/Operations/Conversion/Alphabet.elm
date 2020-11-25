{-
   Operations/Conversion/Alphabet.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to convert Alphabets
-}


module Operations.Conversion.Alphabet exposing (nonDeterministicToDeterministic)

import Models.Alphabet as Alphabet



{- Convert a non deterministic alphabet to a deterministic alphabet -}


nonDeterministicToDeterministic :
    Alphabet.NonDeterministicAlphabet
    -> Alphabet.DeterministicAlphabet
nonDeterministicToDeterministic ndAlphabet =
    case ndAlphabet of
        Alphabet.NDA symbols epsilon ->
            symbols
