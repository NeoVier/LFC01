{-
   Parsing/Common.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to help parse common items like symbols
-}


module Parsing.Common exposing (..)

import Models.Alphabet as Alphabet
import Parser as P exposing ((|.), (|=), Parser)



{- Parse a single character string -}


createChar : String -> Parser Char
createChar xs =
    case String.toList xs |> List.head of
        Just x ->
            P.succeed x

        Nothing ->
            P.problem "invalid symbol"



{- Parse a Symbol -}


alphabetSymbol : Parser Alphabet.Symbol
alphabetSymbol =
    P.oneOf
        [ alphabetGroup
        , alphabetSingle
        ]



{- Parse a Symbol.Single -}


alphabetSingle : Parser Alphabet.Symbol
alphabetSingle =
    singleChar
        |> P.map Alphabet.Single



{- Parse a Symbol.Group -}


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



{- Parse a single alpha-numeric Char -}


singleChar : Parser Char
singleChar =
    P.getChompedString (P.chompIf Char.isAlphaNum)
        |> P.andThen createChar



{- Parse a (Char, Char) tuple to use in groups -}


alphabetGroupInner : Parser ( Char, Char )
alphabetGroupInner =
    P.succeed Tuple.pair
        |= singleChar
        |. P.symbol "-"
        |= singleChar



{- Parse a single word (that only contains alpha-numeric Chars) -}


parseWord : Parser String
parseWord =
    P.getChompedString (P.chompWhile Char.isAlphaNum)
