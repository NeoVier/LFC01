module Conversion.Alphabet exposing (nonDeterministicToDeterministic)

import Models.Alphabet as Alphabet


nonDeterministicToDeterministic :
    Alphabet.NonDeterministicAlphabet
    -> Alphabet.DeterministicAlphabet
nonDeterministicToDeterministic ndAlphabet =
    case ndAlphabet of
        Alphabet.NDA symbols epsilon ->
            symbols
