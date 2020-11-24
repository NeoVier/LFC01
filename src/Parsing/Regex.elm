{-
   Parsing/Regex.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to parse Regexes
-}


module Parsing.Regex exposing (parseRegex)

import Models.Alphabet as Alphabet
import Models.Regex as Regex exposing (Regex(..))
import Parser as P exposing ((|.), (|=), Parser)
import Parsing.Common as PC
import Result.Extra as ResultE
import Utils.Utils as Utils



-- GENERAL
{- Parse a Regex file -}


parseRegex : String -> Maybe (List Regex.IdRegex)
parseRegex content =
    String.lines content
        |> List.filter (not << String.isEmpty)
        |> ResultE.combineMap (P.run regexLine)
        |> Result.toMaybe
        |> Maybe.map joinAllIds


joinAllIds : List Regex.IdRegex -> List Regex.IdRegex
joinAllIds regexes =
    List.map (\( id, r ) -> ( id, joinIdsSingle regexes r )) regexes


joinIdsSingle : List Regex.IdRegex -> Regex.Regex -> Regex.Regex
joinIdsSingle regexes base =
    List.foldl (\( id, r ) acc -> replaceWord id acc r) base regexes


replaceWord : String -> Regex -> Regex -> Regex
replaceWord word r new =
    case r of
        Union c1 c2 ->
            Union (replaceWord word c1 new) (replaceWord word c2 new)

        Concat c1 c2 ->
            case getWord r of
                Nothing ->
                    Concat (replaceWord word c1 new) (replaceWord word c2 new)

                Just w ->
                    if w == word then
                        new

                    else
                        Concat (replaceWord word c1 new)
                            (replaceWord word c2 new)

        Star c1 ->
            Star (replaceWord word c1 new)

        Plus c1 ->
            Plus (replaceWord word c1 new)

        Question c1 ->
            Question (replaceWord word c1 new)

        _ ->
            r



{- Get a possible word from a regex node -}


getWord : Regex.Regex -> Maybe String
getWord node =
    case node of
        Regex.Concat (Regex.Symbol (Alphabet.Single s1)) c2 ->
            case getWord c2 of
                Just w ->
                    Just <| String.fromChar s1 ++ w

                Nothing ->
                    Just <| String.fromChar s1

        Concat c1 (Symbol (Alphabet.Single s2)) ->
            case getWord c1 of
                Just w ->
                    Just <| w ++ String.fromChar s2

                Nothing ->
                    Just <| String.fromChar s2

        Regex.Symbol (Alphabet.Single s1) ->
            Just <| String.fromChar s1

        _ ->
            Nothing



{- Gets all possible words from a regex -}


getAllWords : Regex.Regex -> List String
getAllWords r =
    case r of
        Regex.Union c1 c2 ->
            getAllWords c1 ++ getAllWords c2

        Regex.Concat c1 c2 ->
            let
                thisWord =
                    getWord r
                        |> Maybe.map (\x -> [ x ])
                        |> Maybe.withDefault []
            in
            thisWord ++ getAllWords c1 ++ getAllWords c2

        Regex.Star c1 ->
            getAllWords c1

        Regex.Plus c1 ->
            getAllWords c1

        Regex.Question c1 ->
            getAllWords c1

        _ ->
            []



{- Parse a regex line -}


regexLine : Parser Regex.IdRegex
regexLine =
    P.succeed (\id expression -> ( id, expression ))
        |= regexId
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= regex
        |. P.spaces



{- Parse the Id part of a line -}


regexId : Parser String
regexId =
    P.getChompedString (P.chompWhile (\s -> s /= ':'))



{- Parse a regex -}


regex : Parser Regex
regex =
    let
        base =
            { start = ""
            , separator = "|"
            , end = ""
            , spaces = P.spaces
            , item = union
            , trailing = P.Optional
            }
    in
    P.oneOf
        [ P.sequence base
        , P.sequence { base | start = "(", end = ")" }
        ]
        |> P.andThen (joinWithMaybe Union "regex")



-- UNION
{- Parse a union node -}


union : Parser Regex
union =
    let
        base =
            { start = ""
            , separator = "|"
            , end = ""
            , spaces = P.spaces
            , item = concat
            , trailing = P.Forbidden
            }
    in
    P.oneOf
        [ P.sequence base
        , P.sequence { base | start = "(", end = ")" } |> P.backtrackable
        ]
        |> P.andThen (joinWithMaybe Union "union")



-- CONCAT
{- Parse a concat node -}


concat : Parser Regex
concat =
    let
        base =
            { start = ""
            , separator = ""
            , end = ""
            , spaces = P.spaces
            , item = unary
            , trailing = P.Forbidden
            }
    in
    P.oneOf
        [ P.sequence base
        , P.sequence { base | start = "(", end = ")" }
        , P.lazy
            (\_ ->
                P.sequence
                    { base
                        | start = "("
                        , end = ")"
                        , item = concat
                    }
            )
        , P.lazy (\_ -> P.sequence { base | item = concat })
        ]
        |> P.andThen (joinWithMaybe Concat "concat")


joinWithMaybe :
    (Regex -> Regex -> Regex)
    -> String
    -> List Regex
    -> Parser Regex
joinWithMaybe f regexType regexes =
    Maybe.map2 (List.foldl (\acc new -> f new acc))
        (List.head regexes)
        (List.tail regexes)
        |> parserFromMaybe regexType


parserFromMaybe : String -> Maybe a -> Parser a
parserFromMaybe regexType =
    Maybe.map P.succeed
        >> Maybe.withDefault (P.problem ("Invalid " ++ regexType))



-- UNARY
{- Create a unary operator parser -}


createUnary : String -> (Regex -> Regex) -> Parser Regex
createUnary id f =
    let
        defSucc =
            P.backtrackable <| P.succeed f
    in
    P.backtrackable <|
        P.oneOf
            [ defSucc
                |. P.symbol "("
                |= P.oneOf
                    [ P.lazy (\_ -> regex)
                    , regexSymbol
                    ]
                |. P.symbol ")"
                |. P.symbol id
            , defSucc
                |= regexSymbol
                |. P.symbol id
            ]



{- Parse a star node -}


star : Parser Regex
star =
    createUnary "*" Regex.Star



{- Parse a plus node -}


plus : Parser Regex
plus =
    createUnary "+" Regex.Plus



{- Parse a question node -}


question : Parser Regex
question =
    createUnary "?" Regex.Question



{- Parse an epsilon node -}


epsilon : Parser Regex
epsilon =
    P.succeed Regex.Epsilon
        |. P.symbol "&"
        |> wrapWithParens
        |> P.backtrackable



{- Combine all of the unary operators -}


unary : Parser Regex
unary =
    List.map P.backtrackable
        [ epsilon, star, plus, question, regexSymbol ]
        |> P.oneOf



{- Wrap a regex parser with parens -}


wrapWithParens : Parser Regex -> Parser Regex
wrapWithParens p =
    P.succeed identity
        |. P.symbol "("
        |. P.spaces
        |= p
        |. P.spaces
        |. P.symbol ")"



{- Parse a symbol and convert it to a Regex Symbol -}


regexSymbol : Parser Regex
regexSymbol =
    P.map Regex.Symbol PC.alphabetSymbol
