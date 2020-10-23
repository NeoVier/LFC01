module Models.Alphabet exposing (..)

-- type alias Alphabet =
--     List Symbol


type Alphabet
    = Deterministic DeterministicAlphabet
    | NonDeterministic NonDeterministicAlphabet


type alias DeterministicAlphabet =
    List Symbol


type NonDeterministicAlphabet
    = NDA (List Symbol) Epsilon


type Epsilon
    = Epsilon


type alias Symbol =
    String
