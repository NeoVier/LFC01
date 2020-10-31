{-
   Parsing/Grammars.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to parse Grammars
-}


module Parsing.Grammars exposing (parseGR)

import Models.Grammars as Grammars
    exposing
        ( Grammar
        , NonTerminalSymbol
        , Production
        , ProductionBody
        , TerminalSymbol
        )
import Parser as P exposing ((|.), (|=), Parser)
import Parsing.Common as PC
import Result.Extra as ResultE
import Utils.Utils as Utils


parseGR : String -> Maybe Grammars.Grammar
parseGR s =
    let
        results : Result (List P.DeadEnd) (List Production)
        results =
            List.map (P.run parseProduction) (String.lines s)
                |> ResultE.combine
    in
    case results of
        Err _ ->
            Nothing

        Ok productions ->
            let
                nonTerminals : List NonTerminalSymbol
                nonTerminals =
                    List.map .fromSymbol productions
                        |> Utils.removeDuplicates

                terminals : List TerminalSymbol
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
                    }
                )
                (List.head productions)


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
