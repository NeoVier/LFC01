module Parsing.Grammars exposing (parseGR)

import Models.Grammars as Grammars
import Utils.Utils as Utils


parseGR : String -> Maybe Grammars.Grammar
parseGR content =
    let
        nonTerminals =
            String.filter Char.isUpper content
                |> String.toList
                |> Utils.removeDuplicates

        terminals =
            String.filter Char.isLower content
                |> String.toList
                |> Utils.removeDuplicates

        productions =
            String.lines content
                |> List.map parseProduction
                |> Utils.listOfMaybesToMaybeList

        initialSymbol =
            String.toList content
                |> List.head
    in
    Maybe.map2
        (\p i ->
            { nonTerminals = nonTerminals
            , terminals = terminals
            , productions = p
            , initialSymbol = i
            }
        )
        productions
        initialSymbol


parseProduction : String -> Maybe Grammars.Production
parseProduction line =
    let
        splitLine =
            String.split " -> " line

        productionHead =
            List.head splitLine
                |> Maybe.map String.toList
                |> Maybe.andThen List.head

        productionBody =
            List.drop 1 splitLine
                |> List.head
                |> Maybe.map (String.split " | ")
                |> Maybe.map (List.map parseProductionBody)
                |> Maybe.map Utils.listOfMaybesToMaybeList
                |> Utils.maybeMaybeToMaybe
    in
    Maybe.map2 (\h b -> { fromSymbol = h, productions = b })
        productionHead
        productionBody


parseProductionBody : String -> Maybe Grammars.ProductionBody
parseProductionBody sentence =
    let
        asChars =
            String.toList sentence

        consumed =
            case Maybe.map Char.isLower (List.head asChars) of
                Just True ->
                    List.head asChars

                otherwise ->
                    Nothing

        toSymbol =
            case Maybe.map Char.isUpper (List.head (List.drop 1 asChars)) of
                Just True ->
                    List.head (List.drop 1 asChars)

                otherwise ->
                    Nothing
    in
    case consumed of
        Nothing ->
            case toSymbol of
                Nothing ->
                    Nothing

                otherwise ->
                    Just { consumed = consumed, toSymbol = toSymbol }

        otherwise ->
            Just { consumed = consumed, toSymbol = toSymbol }
