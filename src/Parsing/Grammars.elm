{-
   Parsing/Grammars.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to parse Grammars
-}


module Parsing.Grammars exposing (parseGLC, parseGR)

import Models.Grammars as Grammars exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Parsing.Common as PC
import Result.Extra as ResultE
import Utils.Utils as Utils



-- GR
-- Parse a GR


parseGR : String -> Maybe Grammar
parseGR s =
    let
        acceptsEmpty =
            String.lines s
                |> List.head
                |> Maybe.map (String.contains "&")
                |> Maybe.withDefault False

        firstLineWithoutEpsilon =
            String.lines s
                |> List.head
                |> Maybe.map
                    (String.replace "& |" ""
                        >> String.replace "&|" ""
                        >> String.replace "| &" ""
                        >> String.replace "|&" ""
                        >> String.replace "&" ""
                    )
                |> Maybe.withDefault ""

        newLines =
            String.lines s
                |> List.drop 1
                |> (::) firstLineWithoutEpsilon

        results =
            List.map (P.run parseProduction) newLines
                |> ResultE.combine
    in
    case results of
        Err _ ->
            Nothing

        Ok productions ->
            let
                nonTerminals =
                    List.map .fromSymbol productions
                        |> Utils.removeDuplicates

                terminals =
                    List.concatMap .productions productions
                        |> List.map .consumed
                        |> Utils.removeDuplicates
            in
            Maybe.map
                (\h ->
                    { nonTerminals = nonTerminals
                    , terminals = terminals
                    , productions = productions
                    , initialSymbol = h.fromSymbol
                    , acceptsEmpty = acceptsEmpty
                    }
                )
                (List.head productions)



-- Parse a production


parseProduction : Parser Production
parseProduction =
    P.succeed Production
        |= PC.parseWord
        |. P.spaces
        |. P.symbol "->"
        |. P.spaces
        |= P.sequence
            { start = ""
            , separator = "|"
            , end = ""
            , spaces = P.spaces
            , item = parseProductionBody
            , trailing = P.Forbidden
            }



-- Parse a production body


parseProductionBody : Parser ProductionBody
parseProductionBody =
    P.oneOf
        [ P.backtrackable <|
            P.succeed
                (\c t ->
                    if t == "" then
                        ProductionBody c Nothing

                    else
                        ProductionBody c (Just t)
                )
                |= PC.alphabetSymbol
                |= PC.parseWord
        , P.succeed (\c -> ProductionBody c Nothing)
            |= PC.alphabetSymbol
        ]



-- GLC
-- Parse a GLC


parseGLC : String -> Maybe ContextFreeGrammar
parseGLC s =
    let
        results =
            List.map (P.run contextFreeProduction) (String.lines s)
                |> ResultE.combine
    in
    case results of
        Err _ ->
            Nothing

        Ok productions ->
            let
                nonTerminals =
                    List.map .fromSymbol productions |> Utils.removeDuplicates

                terminals =
                    List.concatMap .productions productions
                        |> List.concat
                        |> List.filterMap getAsTerminal
                        |> Utils.removeDuplicates
            in
            Maybe.map
                (\h ->
                    { nonTerminals = nonTerminals
                    , terminals = terminals
                    , productions = productions
                    , initialSymbol = h.fromSymbol
                    }
                )
                (List.head productions)


getAsTerminal : ContextFreeProductionItem -> Maybe TerminalSymbol
getAsTerminal x =
    case x of
        Terminal t ->
            Just t

        _ ->
            Nothing



-- Parse a Terminal Symbol


terminal : Parser TerminalSymbol
terminal =
    PC.alphabetSymbol



-- Parse a Non Terminal Symbol


nonTerminal : Parser NonTerminalSymbol
nonTerminal =
    P.getChompedString (P.chompIf (\x -> Char.isAlphaNum x && Char.isUpper x))



-- Parse a Production Item


contextFreeProductionItem : Parser ContextFreeProductionItem
contextFreeProductionItem =
    P.oneOf
        [ nonTerminal |> P.map NonTerminal
        , terminal |> P.map Terminal
        ]



-- Parse a Production Body


contextFreeProductionBody : Parser ContextFreeProductionBody
contextFreeProductionBody =
    P.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = P.spaces
        , item = contextFreeProductionItem
        , trailing = P.Optional
        }



-- Parse a Production


contextFreeProduction : Parser ContextFreeProduction
contextFreeProduction =
    P.succeed
        (\from productions ->
            { fromSymbol = from
            , productions = productions
            }
        )
        |= nonTerminal
        |. P.spaces
        |. P.symbol "->"
        |. P.spaces
        |= P.sequence
            { start = ""
            , separator = "|"
            , end = ""
            , spaces = P.spaces
            , item = contextFreeProductionBody
            , trailing = P.Forbidden
            }
