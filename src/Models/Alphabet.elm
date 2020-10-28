{-
   Models/Alphabet.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the definition of everything related to Alphabets
-}


module Models.Alphabet exposing (..)

-- General Alphabet type


type Alphabet
    = Deterministic DeterministicAlphabet
    | NonDeterministic NonDeterministicAlphabet



-- A deterministic alphabet is just a set of Symbols


type alias DeterministicAlphabet =
    List Symbol



-- A non deterministic alphabet is a set of symbols + Epsilon


type NonDeterministicAlphabet
    = NDA (List Symbol) Epsilon



-- Epsilon is just a constant


type Epsilon
    = Epsilon



-- A symbol is just a Char


type alias Symbol =
    Char
