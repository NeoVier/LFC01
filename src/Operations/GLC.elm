{-
   Operations/GLC.elm
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains functions to operate on GLCs
-}


module Operations.GLC exposing (..)

import Dict exposing (Dict)
import Models.Alphabet as Alphabet
import Models.Grammars as Grammars
import Utils.Utils as Utils



-- REMOVE EPSILON
-- Alias NullableInfo


type alias NullableInfo =
    Dict Grammars.NonTerminalSymbol Bool



-- Remove epsilon


removeEpsilon : Grammars.ContextFreeGrammar -> Grammars.ContextFreeGrammar
removeEpsilon glc =
    let
        nullables =
            allNullables glc

        newInitial =
            "NoEpsilonStart"

        newProductions =
            List.map
                (\production ->
                    { production
                        | productions =
                            List.concatMap (newBodies glc nullables)
                                production.productions
                    }
                )
                glc.productions
    in
    if List.member glc.initialSymbol nullables then
        { glc
            | initialSymbol = newInitial
            , productions =
                { fromSymbol = newInitial
                , productions =
                    [ [ Grammars.NonTerminal glc.initialSymbol ]
                    , []
                    ]
                }
                    :: newProductions
            , nonTerminals = newInitial :: glc.nonTerminals
        }

    else
        { glc | productions = newProductions }



-- Generate new bodies for an Epsilon-free glc


newBodies :
    Grammars.ContextFreeGrammar
    -> List Grammars.NonTerminalSymbol
    -> Grammars.ContextFreeProductionBody
    -> List Grammars.ContextFreeProductionBody
newBodies glc nullables originalBody =
    conditionalTree originalBody
        (\item ->
            case item of
                Grammars.Terminal _ ->
                    False

                Grammars.NonTerminal symbol ->
                    List.member symbol nullables
        )
        |> List.filter (not << List.isEmpty)



{- Generate a binary tree where, for each node, the current element is excluded
   (left branch) and included (right branch) if the function evaluates to True.
   Else,  it just goes to the next element.
   This is useful in this context to generate new production bodies.
   e.g. Assume A and D are nullable. Given the input ABcD should generate:
             ABcD
          /        \
        BcD        ABcD
         |          |
        BcD        ABcD
         |          |
        BcD        ABcD
       /   \      /   \
      Bc   BcD  ABc   ABcD

   If you look at the leaf nodes, you should notice that they are exactly the
   new bodies.
-}


conditionalTree : List a -> (a -> Bool) -> List (List a)
conditionalTree l f =
    case List.head l of
        Nothing ->
            [ [] ]

        Just h ->
            let
                tail =
                    List.tail l
                        |> Maybe.withDefault []

                nextTree =
                    conditionalTree tail f
            in
            if f h then
                nextTree
                    ++ List.map (\node -> h :: node) nextTree

            else
                List.map (\node -> h :: node) nextTree



-- Get all the nullable NonTerminals in a glc


allNullables : Grammars.ContextFreeGrammar -> List Grammars.NonTerminalSymbol
allNullables glc =
    allNullablesHelp glc []



-- Auxiliary function


allNullablesHelp :
    Grammars.ContextFreeGrammar
    -> List Grammars.NonTerminalSymbol
    -> List Grammars.NonTerminalSymbol
allNullablesHelp glc seen =
    let
        newSeen =
            List.filterMap
                (\production ->
                    if
                        List.any
                            (List.all
                                (\symbol ->
                                    case symbol of
                                        Grammars.Terminal _ ->
                                            False

                                        Grammars.NonTerminal nonTerminal ->
                                            List.member nonTerminal seen
                                )
                            )
                            production.productions
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
        allNullablesHelp glc newSeen
