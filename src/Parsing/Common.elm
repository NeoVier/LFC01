module Parsing.Common exposing (..)

import Models.Alphabet as Alphabet
import Parser as P exposing ((|.), (|=), Parser)


createChar : String -> Parser Char
createChar xs =
    case String.toList xs |> List.head of
        Just x ->
            P.succeed x

        Nothing ->
            P.problem "invalid symbol"


alphabetSymbol : Parser Alphabet.Symbol
alphabetSymbol =
    P.oneOf
        [ alphabetGroup
        , alphabetSingle
        ]


alphabetSingle : Parser Alphabet.Symbol
alphabetSingle =
    singleChar
        |> P.map Alphabet.Single


alphabetGroup : Parser Alphabet.Symbol
alphabetGroup =
    P.sequence
        { start = "["
        , separator = ""
        , end = "]"
        , spaces = P.spaces
        , item = alphabetGroupInner
        , trailing = P.Forbidden
        }
        |> P.map Alphabet.Group


singleChar : Parser Char
singleChar =
    P.getChompedString (P.chompIf Char.isAlphaNum)
        |> P.andThen createChar


alphabetGroupInner : Parser ( Char, Char )
alphabetGroupInner =
    P.succeed Tuple.pair
        |= singleChar
        |. P.symbol "-"
        |= singleChar


parseWord : Parser String
parseWord =
    P.getChompedString (P.chompWhile Char.isAlphaNum)
