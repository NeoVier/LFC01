{-
   Saving/Regex.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to save Regexes
-}


module Saving.Regex exposing (idRegexToString, regexToString)

import Models.Alphabet as Alphabet
import Models.Regex as Regex exposing (Regex(..))



{- Transform an IdRegex into a string -}


idRegexToString : Regex.IdRegex -> String
idRegexToString idR =
    case idR of
        ( id, r ) ->
            id ++ ": " ++ regexToString r



{- Transform a regex into a string -}


regexToString : Regex -> String
regexToString r =
    case r of
        Epsilon ->
            "&"

        Symbol s ->
            symbolToString s

        Union r1 r2 ->
            "(" ++ regexToString r1 ++ " | " ++ regexToString r2 ++ ")"

        Concat r1 r2 ->
            regexToString r1 ++ regexToString r2

        Star r1 ->
            if isSymbolEpsilonOrUnion r1 then
                regexToString r1 ++ "*"

            else
                "(" ++ regexToString r1 ++ ")" ++ "*"

        Plus r1 ->
            if isSymbolEpsilonOrUnion r1 then
                regexToString r1 ++ "+"

            else
                "(" ++ regexToString r1 ++ ")" ++ "+"

        Question r1 ->
            if isSymbolEpsilonOrUnion r1 then
                regexToString r1 ++ "?"

            else
                "(" ++ regexToString r1 ++ ")" ++ "?"



{- Check if a symbol is an Union node or an Epsilon node -}


isSymbolEpsilonOrUnion : Regex -> Bool
isSymbolEpsilonOrUnion r =
    case r of
        Epsilon ->
            True

        Symbol _ ->
            True

        Union _ _ ->
            True

        _ ->
            False



{- Transform a symbol into a string -}


symbolToString : Alphabet.Symbol -> String
symbolToString s =
    case s of
        Alphabet.Single c ->
            String.fromChar c

        Alphabet.Group g ->
            "["
                ++ String.join ""
                    (List.map
                        (\( c1, c2 ) ->
                            String.fromList [ c1, '-', c2 ]
                        )
                        g
                    )
                ++ "]"
