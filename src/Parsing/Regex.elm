module Parsing.Regex exposing (regexFile)

import Models.Alphabet as Alphabet
import Models.Regex as Regex exposing (Regex)
import Parser as P exposing ((|.), (|=), Parser)
import Parsing.Common as PC
import Result.Extra as ResultE
import Utils.Utils as Utils



-- GENERAL


regexFile : String -> Result (List P.DeadEnd) (List Regex.IdRegex)
regexFile content =
    ResultE.combineMap (P.run regexLine) (String.lines content)


regexLine : Parser Regex.IdRegex
regexLine =
    P.succeed (\id expression -> ( id, expression ))
        |= regexId
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= regex


regexId : Parser String
regexId =
    P.getChompedString (P.chompWhile (\s -> s /= ':'))


regex : Parser Regex
regex =
    P.oneOf
        [ P.backtrackable <| union
        , P.backtrackable <| concat
        , P.backtrackable <| unary
        ]
        |. P.oneOf [ P.end, P.symbol "\n" ]



-- CONCAT


concat : Parser Regex
concat =
    P.loop Nothing concatHelp


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


baseUnion : Parser Regex
baseUnion =
    P.backtrackable <|
        P.succeed Regex.Union
            |= concat
            |. P.spaces
            |. P.symbol "|"
            |. P.spaces
            |= concat



-- UNARY


createUnary : String -> (Regex -> Regex) -> Parser Regex
createUnary id f =
    let
        defSucc =
            P.succeed f
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


star : Parser Regex
star =
    createUnary "*" Regex.Star


plus : Parser Regex
plus =
    createUnary "+" Regex.Plus


question : Parser Regex
question =
    createUnary "?" Regex.Question


epsilon : Parser Regex
epsilon =
    P.succeed Regex.Epsilon
        |. P.symbol "&"


unary : Parser Regex
unary =
    P.oneOf [ epsilon, star, plus, question, regexSymbol ]


regexSymbol : Parser Regex
regexSymbol =
    P.map Regex.Symbol PC.alphabetSymbol
