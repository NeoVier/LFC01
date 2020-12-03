{-
   Utils/GLC.elm
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains util functions related to GLCs
-}


module Utils.GLC exposing
    ( contextFreeProductionBodyToString
    , contextFreeProductionItemToString
    , getNextName
    , nullables
    , productionFromBodies
    , productionFromSymbol
    , terminalToNonTerminal
    )

import Models.Alphabet exposing (..)
import Models.Grammars exposing (..)
import Utils.Utils as Utils



{- Convert a TerminalSymbol to a NonTerminalSymbol -}


terminalToNonTerminal : TerminalSymbol -> NonTerminalSymbol
terminalToNonTerminal t =
    case t of
        Single c ->
            Char.toUpper c |> String.fromChar

        Group g ->
            Utils.symbolToString (Group g)



{- Transform a Context Free Production Body into a String -}


contextFreeProductionBodyToString : ContextFreeProductionBody -> String
contextFreeProductionBodyToString =
    List.map contextFreeProductionItemToString >> String.join ""



{- Transform a Context Free Production Item into a String -}


contextFreeProductionItemToString : ContextFreeProductionItem -> String
contextFreeProductionItemToString x =
    case x of
        Terminal t ->
            Utils.symbolToString t

        NonTerminal nt ->
            nt



{- Get the next available name, given a symbol
   (e.g. S -> S', S' -> S'', A -> A', ...)
-}


getNextName : ContextFreeGrammar -> NonTerminalSymbol -> NonTerminalSymbol
getNextName glc symbol =
    let
        nextName =
            symbol ++ "'"
    in
    if not (List.member symbol glc.nonTerminals) then
        symbol

    else if List.member nextName glc.nonTerminals then
        getNextName glc nextName

    else
        nextName



{- Get all the nullable NonTerminals in a glc -}


nullables : ContextFreeGrammar -> List NonTerminalSymbol
nullables glc =
    nullablesHelp glc []



{- Auxiliary function -}


nullablesHelp :
    ContextFreeGrammar
    -> List NonTerminalSymbol
    -> List NonTerminalSymbol
nullablesHelp glc seen =
    let
        newSeen =
            List.filterMap
                (\production ->
                    if
                        List.any
                            (List.all
                                (\symbol ->
                                    case symbol of
                                        Terminal _ ->
                                            False

                                        NonTerminal nonTerminal ->
                                            List.member nonTerminal seen
                                )
                            )
                            production.bodies
                    then
                        Just production

                    else
                        Nothing
                )
                glc.productions
                |> List.map .fromSymbol
    in
    if newSeen == seen then
        seen

    else
        nullablesHelp glc newSeen


productionFromSymbol :
    ContextFreeGrammar
    -> NonTerminalSymbol
    -> Maybe ContextFreeProduction
productionFromSymbol glc nt =
    List.filter (.fromSymbol >> (==) nt) glc.productions
        |> List.head


productionFromBodies :
    ContextFreeGrammar
    -> List ContextFreeProductionBody
    -> Maybe ContextFreeProduction
productionFromBodies glc bodies =
    List.filter (.bodies >> (==) bodies) glc.productions
        |> List.head
