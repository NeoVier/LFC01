{-
   Operations/GLC.elm
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains functions to operate on GLCs
-}


module Operations.GLC exposing (..)

import Dict exposing (Dict)
import Html exposing (th)
import Models.Alphabet as Alphabet
import Models.Grammars as Grammars exposing (..)
import Utils.Utils as Utils



-- REMOVE EPSILON
-- Alias NullableInfo


type alias NullableInfo =
    Dict NonTerminalSymbol Bool



-- Remove epsilon


removeEpsilon : ContextFreeGrammar -> ContextFreeGrammar
removeEpsilon glc =
    let
        nullables =
            allNullables glc

        newInitial =
            getNextName glc glc.initialSymbol

        newProductions =
            List.map
                (\production ->
                    { production
                        | bodies =
                            List.concatMap (newBodies glc nullables)
                                production.bodies
                    }
                )
                glc.productions
    in
    if List.member glc.initialSymbol nullables then
        { glc
            | initialSymbol = newInitial
            , productions =
                { fromSymbol = newInitial
                , bodies =
                    [ [ NonTerminal glc.initialSymbol ]
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
    ContextFreeGrammar
    -> List NonTerminalSymbol
    -> ContextFreeProductionBody
    -> List ContextFreeProductionBody
newBodies glc nullables originalBody =
    conditionalTree originalBody
        (\item ->
            case item of
                Terminal _ ->
                    False

                NonTerminal symbol ->
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


allNullables : ContextFreeGrammar -> List NonTerminalSymbol
allNullables glc =
    allNullablesHelp glc []



-- Auxiliary function


allNullablesHelp :
    ContextFreeGrammar
    -> List NonTerminalSymbol
    -> List NonTerminalSymbol
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
        allNullablesHelp glc newSeen



-- LEFT RECURSION
-- Eliminate left recursion from a GLC


eliminateLeftRecursion : ContextFreeGrammar -> ContextFreeGrammar
eliminateLeftRecursion glc =
    removeEpsilon glc



-- Given a GLC, return the same GLC, but without direct left recursion
-- (useful for testing)


eliminateAllLeftRecursionDirect : ContextFreeGrammar -> ContextFreeGrammar
eliminateAllLeftRecursionDirect glc =
    List.foldr (\prod accGlc -> eliminateLeftRecursionDirect accGlc prod)
        glc
        glc.productions



-- Given a GLC and a production, return a GLC that removed the left recursion
-- from that production


eliminateLeftRecursionDirect :
    ContextFreeGrammar
    -> ContextFreeProduction
    -> ContextFreeGrammar
eliminateLeftRecursionDirect glc prod =
    let
        recursiveBodies =
            List.filter (isDirectlyRecursive prod.fromSymbol) prod.bodies
                |> List.map (List.drop 1)

        nonRecursiveBodies =
            List.filter (not << isDirectlyRecursive prod.fromSymbol)
                prod.bodies

        nextSymbol =
            getNextName glc prod.fromSymbol

        replacementProduction =
            { prod
                | bodies =
                    List.map (\body -> body ++ [ NonTerminal nextSymbol ])
                        nonRecursiveBodies
            }

        newProduction =
            { fromSymbol = nextSymbol
            , bodies =
                List.map (\body -> body ++ [ NonTerminal nextSymbol ])
                    recursiveBodies
                    ++ [ [] ]
            }
    in
    if List.isEmpty recursiveBodies then
        glc

    else
        { glc
            | productions =
                Utils.replaceBy prod replacementProduction glc.productions
                    |> Utils.insertAfter replacementProduction newProduction
            , nonTerminals =
                Utils.insertAfter prod.fromSymbol
                    nextSymbol
                    glc.nonTerminals
        }



-- Check if a body (given the symbol it comes from) is left recursive


isDirectlyRecursive : NonTerminalSymbol -> ContextFreeProductionBody -> Bool
isDirectlyRecursive fromSymbol body =
    case List.head body of
        Just (NonTerminal h) ->
            fromSymbol == h

        _ ->
            False



-- Get the next available name, given a symbol (S -> S', S' -> S'', A -> A', ..)


getNextName : ContextFreeGrammar -> NonTerminalSymbol -> NonTerminalSymbol
getNextName glc symbol =
    let
        nextName =
            symbol ++ "'"
    in
    if List.member nextName glc.nonTerminals then
        getNextName glc nextName

    else
        nextName



{- Para i := 1 até n faça
   | Para j:= 1 até i-1 faça
   |  | Se Ai ::= Ajα ∈ P então
   |  |  | Remova Ai ::= Aj de P
   |  |  | Se Aj ::= β ∈ P então
   |  |  |  | P′ = P′ ∪ {Ai::=βα}
   |  Elimine as recursões diretas das produções de P′com lado esquerdo Ai


   For each production p1 in glc.productions:
   |  For each production p2 before p1:
   |  | If p1 derives p2 (p1 ::= p2+[alpha]) then
   |  |  | Remove p1 ::= p2+[alpha] from glc.productions
   |  |  | For each body in p2:
   |  |  |  | If p2 ::= [beta]:
   |  |  |  |  | p1.productions ++ [[beta] ++ [alfa]]
   | Remove direct left recursion from p1
-}


isIndirectlyRecursive : ContextFreeGrammar -> NonTerminalSymbol -> Bool
isIndirectlyRecursive glc symbol =
    let
        production =
            List.filter (.fromSymbol >> (==) symbol) glc.productions
                |> List.head
    in
    case production of
        Nothing ->
            False

        Just p ->
            let
                derivables =
                    List.filterMap
                        (\body ->
                            case List.head body of
                                Just (NonTerminal nt) ->
                                    Just nt

                                _ ->
                                    Nothing
                        )
                        p.bodies
            in
            isIndirectlyRecursiveHelp glc symbol [ symbol ] derivables


isIndirectlyRecursiveHelp :
    ContextFreeGrammar
    -> NonTerminalSymbol
    -> List NonTerminalSymbol
    -> List NonTerminalSymbol
    -> Bool
isIndirectlyRecursiveHelp glc target seen unseen =
    case List.head unseen of
        Nothing ->
            False

        Just curr ->
            if List.member curr seen then
                isIndirectlyRecursiveHelp glc target seen (List.drop 1 unseen)

            else
                let
                    maybeProduction =
                        List.filter (.fromSymbol >> (==) curr) glc.productions
                            |> List.head
                in
                case maybeProduction of
                    Nothing ->
                        False

                    Just production ->
                        let
                            derivables =
                                List.filterMap
                                    (\body ->
                                        case List.head body of
                                            Just (NonTerminal nt) ->
                                                Just nt

                                            _ ->
                                                Nothing
                                    )
                                    production.bodies
                        in
                        if List.member target derivables then
                            True

                        else
                            isIndirectlyRecursiveHelp glc
                                target
                                (curr :: seen)
                                (List.drop 1 unseen ++ derivables)
