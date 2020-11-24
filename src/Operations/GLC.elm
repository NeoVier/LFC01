{-
   Operations/GLC.elm
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains functions to operate on GLCs
-}


module Operations.GLC exposing (..)

import GenericDict as Dict exposing (Dict)
import Models.Alphabet exposing (Symbol(..))
import Models.Grammars as Grammars exposing (..)
import Utils.Utils as Utils



-- REMOVE EPSILON
{- Alias NullableInfo -}


type alias NullableInfo =
    Dict NonTerminalSymbol Bool



{- Remove epsilon -}


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
                            List.concatMap (newEpsilonFreeBodies glc nullables)
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



{- Generate new bodies for an Epsilon-free glc -}


newEpsilonFreeBodies :
    ContextFreeGrammar
    -> List NonTerminalSymbol
    -> ContextFreeProductionBody
    -> List ContextFreeProductionBody
newEpsilonFreeBodies glc nullables originalBody =
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



{- Get all the nullable NonTerminals in a glc -}


allNullables : ContextFreeGrammar -> List NonTerminalSymbol
allNullables glc =
    allNullablesHelp glc []



{- Auxiliary function -}


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
{- Eliminate left recursion from a GLC -}


eliminateLeftRecursion : ContextFreeGrammar -> ContextFreeGrammar
eliminateLeftRecursion glc =
    let
        withoutEpsilon =
            removeEpsilon glc

        step =
            List.foldl
                (\prod accGlc ->
                    eliminateLeftRecursionIndirect accGlc prod
                )
                withoutEpsilon
                withoutEpsilon.productions
    in
    if List.any (isIndirectlyRecursive glc) glc.nonTerminals then
        eliminateLeftRecursion step

    else
        eliminateAllLeftRecursionDirect glc



{- Given a GLC, return the same GLC, but without direct left recursion -}


eliminateAllLeftRecursionDirect : ContextFreeGrammar -> ContextFreeGrammar
eliminateAllLeftRecursionDirect glc =
    List.foldr (\prod accGlc -> eliminateLeftRecursionDirect accGlc prod)
        glc
        glc.productions



{- Given a GLC and a production, return a GLC that removed the direct left
   recursion from that production
-}


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



{- Check if a body (given the symbol it comes from) is left recursive -}


isDirectlyRecursive : NonTerminalSymbol -> ContextFreeProductionBody -> Bool
isDirectlyRecursive fromSymbol body =
    case List.head body of
        Just (NonTerminal h) ->
            fromSymbol == h

        _ ->
            False



{- Get the next available name, given a symbol
   (e.g. S -> S', S' -> S'', A -> A', ...)
-}


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



{- Given a GLC and a production, return a GLC that removed the indirect left
   recursion from that production
-}


eliminateLeftRecursionIndirect :
    ContextFreeGrammar
    -> ContextFreeProduction
    -> ContextFreeGrammar
eliminateLeftRecursionIndirect glc prod =
    if not <| isIndirectlyRecursive glc prod.fromSymbol then
        glc

    else
        let
            p2s : List ContextFreeProduction
            p2s =
                Utils.takeUntil prod glc.productions

            ( newProd, newGlc ) =
                List.foldl
                    (\currProd ( accProd, accGlc ) ->
                        let
                            -- Every body in p1 that left-most derives p2
                            recursiveBodies =
                                List.filter (isDirectlyRecursive currProd.fromSymbol)
                                    accProd.bodies

                            -- Replace items in p2 into p1
                            newRecursiveBodies =
                                List.concatMap
                                    (\beta ->
                                        List.map (\alpha -> alpha ++ List.drop 1 beta)
                                            currProd.bodies
                                    )
                                    recursiveBodies

                            -- Generate new bodies
                            newBodies =
                                List.filter
                                    (not << isDirectlyRecursive currProd.fromSymbol)
                                    accProd.bodies
                                    ++ newRecursiveBodies
                                    |> Utils.removeDuplicates

                            newProduction =
                                { accProd | bodies = newBodies }
                        in
                        if List.isEmpty recursiveBodies then
                            -- If p1 doesn't derive p2, continue
                            ( accProd, accGlc )

                        else
                            -- Else, replace the production with the new
                            -- production
                            ( newProduction
                            , { accGlc
                                | productions =
                                    Utils.replaceBy accProd
                                        newProduction
                                        accGlc.productions
                              }
                            )
                    )
                    ( prod, glc )
                    p2s
        in
        eliminateLeftRecursionDirect newGlc newProd



{- Checks whether or not a production with fromSymbol == symbol is indirectly
   left recursive
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



{- Helper function -}


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



-- FACTORATION
{- Factor GLC -}


factorGLC : ContextFreeGrammar -> ContextFreeGrammar
factorGLC glc =
    glc



-- If a body starts with a nonterminal, substitute the nonterminal by its productions
-- factorIndirectly
{- Remove direct non determinism from a production -}


factorDirectly :
    ContextFreeGrammar
    -> ContextFreeProduction
    -> ContextFreeGrammar
factorDirectly glc production =
    let
        groups =
            getBiggestBlobs production.bodies
    in
    Dict.fold
        (\group accBodies accGlc ->
            let
                replacementProduction =
                    { production
                        | bodies =
                            (group ++ [ NonTerminal newProduction.fromSymbol ])
                                :: List.filter
                                    (\b -> not <| List.member b accBodies)
                                    production.bodies
                    }

                newProduction =
                    { fromSymbol = getNextName accGlc production.fromSymbol
                    , bodies =
                        List.map (List.drop (List.length group))
                            accBodies
                    }
            in
            if List.length newProduction.bodies == 0 then
                accGlc

            else
                { accGlc
                    | productions =
                        Utils.replaceBy production
                            replacementProduction
                            accGlc.productions
                            |> Utils.insertAfter replacementProduction newProduction
                    , nonTerminals =
                        Utils.insertAfter replacementProduction.fromSymbol
                            newProduction.fromSymbol
                            accGlc.nonTerminals
                }
        )
        glc
        groups



{- Create groups of production bodies that start with the same N symbols -}


joinByFirstN :
    List ContextFreeProductionBody
    -> Int
    -> Dict ContextFreeProductionBody (List ContextFreeProductionBody)
joinByFirstN bodies n =
    List.foldl
        (\body accDict ->
            let
                key =
                    List.take n body
            in
            case Dict.get contextFreeProductionBodyToString key accDict of
                Nothing ->
                    Dict.insert contextFreeProductionBodyToString
                        key
                        [ body ]
                        accDict

                Just v ->
                    Dict.insert contextFreeProductionBodyToString
                        key
                        (v ++ [ body ])
                        accDict
        )
        Dict.empty
        bodies



{- Separate elements in the list that have the same starting chain, keeping the
   biggest chain possible. Keys in the dict are the chains, and the values
   are the entire elements
-}


getBiggestBlobs :
    List ContextFreeProductionBody
    -> Dict ContextFreeProductionBody (List ContextFreeProductionBody)
getBiggestBlobs l =
    let
        initialDict =
            joinByFirstN l 1
    in
    initialDict
        |> Dict.toList
        |> List.map (\( k, v ) -> getBiggestBlobsHelp k v)
        |> Dict.fromList contextFreeProductionBodyToString



{- Gets the biggest possible chain of elements that is repeated in every single
   element in value
-}


getBiggestBlobsHelp :
    ContextFreeProductionBody
    -> List ContextFreeProductionBody
    -> ( ContextFreeProductionBody, List ContextFreeProductionBody )
getBiggestBlobsHelp key value =
    List.foldl
        (\n ( accKey, accValue ) ->
            let
                newPairs =
                    joinByFirstN accValue n |> Dict.toList
            in
            if List.length newPairs == 1 then
                List.head newPairs |> Maybe.withDefault ( accKey, accValue )

            else
                ( accKey, accValue )
        )
        ( key, value )
        (List.range 0
            (List.map List.length value
                |> List.maximum
                |> Maybe.withDefault 0
            )
        )



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
