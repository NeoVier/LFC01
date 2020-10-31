module Parsing.Common exposing (..)

import Models.Alphabet as Alphabet
import Parser as P exposing (Parser)


createChar : String -> Parser Char
createChar xs =
    case String.toList xs |> List.head of
        Just x ->
            P.succeed x

        Nothing ->
            P.problem "invalid symbol"


alphabetSymbol : Parser Alphabet.Symbol
alphabetSymbol =
    P.getChompedString (P.chompIf Char.isAlphaNum)
        |> P.andThen createChar
