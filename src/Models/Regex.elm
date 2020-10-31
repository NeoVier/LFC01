module Models.Regex exposing (..)

import Models.Alphabet as Alphabet


type Regex
    = Epsilon
    | Symbol Alphabet.Symbol
    | Union Regex Regex
    | Concat Regex Regex
    | Star Regex
    | Plus Regex
    | Question Regex
    | Group (List GroupInner)


type alias GroupInner =
    ( Alphabet.Symbol, Alphabet.Symbol )


type alias IdRegex =
    ( String, Regex )
