{-
   Parsing/Regex.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to parse Regexes
-}
-- module Parsing.Regex exposing (parseRegex, regex)


module Parsing.Regex exposing (..)

import Models.Alphabet as Alphabet
import Models.Regex as Regex exposing (Regex)
import Parser as P exposing ((|.), (|=), Parser)
import Parsing.Common as PC
import Result.Extra as ResultE
import Utils.Utils as Utils



-- GENERAL
-- Parse a Regex file


parseRegex : String -> Maybe (List Regex.IdRegex)
parseRegex content =
    String.lines content
        |> List.filter (not << String.isEmpty)
        |> ResultE.combineMap (P.run regexLine)
        |> Result.toMaybe



-- Parse a regex line


regexLine : Parser Regex.IdRegex
regexLine =
    P.succeed (\id expression -> ( id, expression ))
        |= regexId
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= regex
        |. P.spaces



-- Parse the Id part of a line


regexId : Parser String
regexId =
    P.getChompedString (P.chompWhile (\s -> s /= ':'))



-- Parse a regex


regex : Parser Regex
regex =
    P.oneOf
        [ P.backtrackable <| concat
        , P.backtrackable <| unary
        , P.backtrackable <| union
        ]
        |. P.oneOf [ P.end, P.symbol "\n" ]



-- CONCAT
-- Parse a concat node


concat : Parser Regex
concat =
    P.loop Nothing concatHelp



-- Helper function for concat


concatHelp : Maybe Regex -> Parser (P.Step (Maybe Regex) Regex)
concatHelp current =
    let
        join s =
            case current of
                Nothing ->
                    P.Loop (Just s)

                Just expression ->
                    P.Loop <| Just <| Regex.Concat expression s
    in
    P.oneOf
        [ P.backtrackable <|
            P.succeed join
                |. P.symbol "("
                |= P.oneOf [ concat, union ]
                |. P.symbol ")"
        , unary
            |> P.map join
        , case current of
            Nothing ->
                P.problem "Invalid RegEx"

            Just expression ->
                P.succeed (P.Done expression)
        ]



-- UNION
-- Parse a union node


union : Parser Regex
union =
    P.oneOf
        [ P.backtrackable <|
            P.succeed Regex.Union
                |= P.oneOf [ baseUnion, concat ]
                |. P.spaces
                |. P.symbol "|"
                |. P.spaces
                |= P.oneOf [ baseUnion, concat ]
        , baseUnion
        ]



-- Parse a single union node


baseUnion : Parser Regex
baseUnion =
    P.backtrackable <|
        P.oneOf
            [ P.succeed Regex.Union
                |. P.symbol "("
                |= concat
                |. P.spaces
                |. P.symbol "|"
                |. P.spaces
                |= concat
                |. P.symbol ")"
            , P.succeed Regex.Union
                |= concat
                |. P.spaces
                |. P.symbol "|"
                |. P.spaces
                |= concat
            ]



-- UNARY
-- Create a unary operator parser


createUnary : String -> (Regex -> Regex) -> Parser Regex
createUnary id f =
    let
        defSucc =
            P.backtrackable <| P.succeed f
    in
    P.backtrackable <|
        P.oneOf
            [ defSucc
                |= regexSymbol
                |. P.symbol id
            , defSucc
                |. P.symbol "("
                |= P.oneOf [ union, concat ]
                |. P.symbol ")"
                |. P.symbol id
            ]



-- Parse a star node


star : Parser Regex
star =
    createUnary "*" Regex.Star



-- Parse a plus node


plus : Parser Regex
plus =
    createUnary "+" Regex.Plus



-- Parse a question node


question : Parser Regex
question =
    createUnary "?" Regex.Question



-- Parse an epsilon node


epsilon : Parser Regex
epsilon =
    P.succeed Regex.Epsilon
        |. P.symbol "&"



-- Combine all of the unary operators


unary : Parser Regex
unary =
    P.oneOf [ epsilon, star, plus, question, regexSymbol ]



-- Parse a symbol and convert it to a Regex Symbol


regexSymbol : Parser Regex
regexSymbol =
    P.map Regex.Symbol PC.alphabetSymbol
