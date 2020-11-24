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

        newInitialProduction =
            { fromSymbol = newInitial
            , bodies =
                [ [ NonTerminal glc.initialSymbol ]
                , []
                ]
            }

        previousInitialProduction =
            List.filter (.fromSymbol >> (==) glc.initialSymbol)
                glc.productions
                |> List.head
    in
    if List.member glc.initialSymbol nullables then
        case previousInitialProduction of
            Just pp ->
                case pp.bodies of
                    [ [ NonTerminal _ ], [] ] ->
                        { glc | productions = newProductions }

                    _ ->
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

            Nothing ->
                glc

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


eliminateLeftRecursion : ContextFreeGrammar -> Result String ContextFreeGrammar
eliminateLeftRecursion glc =
    let
        maxSteps =
            10
    in
    List.foldl
        (\idx ( finished, accGlc ) ->
            case accGlc of
                Ok g ->
                    if finished then
                        if g == glc then
                            ( True, Ok g )

                        else
                            ( True, accGlc )

                    else if idx == maxSteps then
                        ( False
                        , Err "Depois de muitos passos, a gramática não se resolveu, então deve ter entrado em loop"
                        )

                    else
                        let
                            thisStep =
                                eliminateLeftRecursionStep g
                        in
                        ( thisStep == g, Ok thisStep )

                Err x ->
                    ( finished, Err x )
        )
        ( False, Ok glc )
        (List.range 1 maxSteps)
        |> Tuple.second



{- Step to eliminate left recursion from a GLC -}


eliminateLeftRecursionStep : ContextFreeGrammar -> ContextFreeGrammar
eliminateLeftRecursionStep glc =
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
        step

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


factorGLC : ContextFreeGrammar -> Result String ContextFreeGrammar
factorGLC glc =
    let
        maxSteps =
            10

        minFactor =
            5
    in
    List.foldl
        (\idx ( finished, accGlc ) ->
            case accGlc of
                Ok prevGlc ->
                    if finished then
                        if prevGlc == glc then
                            ( True, Ok prevGlc )

                        else
                            ( True, Ok prevGlc )

                    else if idx == maxSteps then
                        ( False
                        , Err
                            "Depois de muitos passos, a gramática não se resolveu, então deve ter entrado em loop"
                        )

                    else
                        let
                            thisStep =
                                factorGLCStep prevGlc
                        in
                        if
                            List.length thisStep.productions
                                > (minFactor * List.length prevGlc.productions)
                        then
                            ( False, Err "A gramática cresceu demais em um único passo, então deve ter entrado em loop" )

                        else
                            ( thisStep == prevGlc, Ok thisStep )

                Err x ->
                    ( finished, Err x )
        )
        ( False, Ok glc )
        (List.range 1 maxSteps)
        |> Tuple.second



{- A step into factoring a GLC -}


factorGLCStep : ContextFreeGrammar -> ContextFreeGrammar
factorGLCStep glc =
    List.foldl
        (\accProd accGlc ->
            let
                indirectlyFactored =
                    factorIndirectly accGlc accProd

                newProd =
                    List.filter (.fromSymbol >> (==) accProd.fromSymbol)
                        indirectlyFactored.productions
                        |> List.head
            in
            case newProd of
                Nothing ->
                    indirectlyFactored

                Just p ->
                    factorDirectly indirectlyFactored p
        )
        glc
        glc.productions



{- Remove indirect non determinism from a production -}


factorIndirectly :
    ContextFreeGrammar
    -> ContextFreeProduction
    -> ContextFreeGrammar
factorIndirectly glc production =
    let
        newBodies : List ContextFreeProductionBody
        newBodies =
            List.concatMap
                (\body ->
                    case List.head body of
                        Just (NonTerminal nt) ->
                            let
                                maybeNtProduction =
                                    List.filter (.fromSymbol >> (==) nt)
                                        glc.productions
                                        |> List.head
                            in
                            case maybeNtProduction of
                                Nothing ->
                                    [ body ]

                                Just ntProd ->
                                    List.map
                                        (\ntBody ->
                                            ntBody
                                                ++ List.drop 1 body
                                        )
                                        ntProd.bodies

                        _ ->
                            [ body ]
                )
                production.bodies

        newProduction =
            { production | bodies = newBodies }
    in
    { glc
        | productions =
            Utils.replaceBy production newProduction glc.productions
    }



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
            if List.length accBodies >= 2 then
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

            else
                accGlc
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



-- USELESS SYMBOLS
{- Remove all useless symbols (unreachable and unproductive) -}


removeUseless : ContextFreeGrammar -> ContextFreeGrammar
removeUseless =
    removeUnproductives >> removeUnreachables >> reassignTerminals



{- Remove all unreachable symbols from a grammar -}


removeUnreachables : ContextFreeGrammar -> ContextFreeGrammar
removeUnreachables glc =
    let
        initialProduction =
            List.filter (.fromSymbol >> (==) glc.initialSymbol) glc.productions
                |> List.head
    in
    case initialProduction of
        Nothing ->
            glc

        Just prod ->
            removeUnreachableHelp glc [] [ prod ]
                |> reassignTerminals



{- Helper function -}


removeUnreachableHelp :
    ContextFreeGrammar
    -> List ContextFreeProduction
    -> List ContextFreeProduction
    -> ContextFreeGrammar
removeUnreachableHelp glc seen unseen =
    case List.head unseen of
        Nothing ->
            { glc
                | productions =
                    List.filter (\p -> List.member p seen) glc.productions
                , nonTerminals =
                    List.filter
                        (\s -> List.member s (List.map .fromSymbol seen))
                        glc.nonTerminals
            }

        Just curr ->
            if List.member curr seen then
                removeUnreachableHelp glc seen (List.drop 1 unseen)

            else
                removeUnreachableHelp glc
                    ((curr :: seen ++ reachables glc curr) |> Utils.removeDuplicates)
                    (List.drop 1 unseen)



{- Get all the reachable symbols from a grammar -}


reachables :
    ContextFreeGrammar
    -> ContextFreeProduction
    -> List ContextFreeProduction
reachables glc prod =
    List.concat prod.bodies
        |> List.filterMap
            (\item ->
                case item of
                    NonTerminal x ->
                        Just x

                    _ ->
                        Nothing
            )
        |> List.filterMap
            (\nt ->
                List.filter (.fromSymbol >> (==) nt) glc.productions
                    |> List.head
            )



{- Remove all unproductive symbols from a grammar -}


removeUnproductives : ContextFreeGrammar -> ContextFreeGrammar
removeUnproductives glc =
    let
        ps =
            productives glc
    in
    { glc
        | nonTerminals = List.filter (\nt -> List.member nt ps) glc.nonTerminals
        , productions =
            List.filterMap
                (\p ->
                    if not (List.member p.fromSymbol ps) then
                        Nothing

                    else
                        Just
                            { p
                                | bodies =
                                    List.filter
                                        (List.all
                                            (\item ->
                                                case item of
                                                    Terminal _ ->
                                                        True

                                                    NonTerminal x ->
                                                        List.member x ps
                                            )
                                        )
                                        p.bodies
                            }
                )
                glc.productions
    }
        |> reassignTerminals



{- Gets all the productive NonTerminals from a grammar -}


productives : ContextFreeGrammar -> List NonTerminalSymbol
productives glc =
    List.foldl
        (\item ( accProductives, accUnproductives ) ->
            if
                productivesHelp glc
                    ( accProductives, accUnproductives )
                    (NonTerminal item)
            then
                ( item :: accProductives, accUnproductives )

            else
                ( accProductives, item :: accUnproductives )
        )
        ( [], [] )
        glc.nonTerminals
        |> Tuple.first



{- Helper function -}


productivesHelp :
    ContextFreeGrammar
    -> ( List NonTerminalSymbol, List NonTerminalSymbol )
    -> ContextFreeProductionItem
    -> Bool
productivesHelp glc ( productiveSymbols, unproductiveSymbols ) item =
    case item of
        Terminal _ ->
            True

        NonTerminal nt ->
            if List.member nt productiveSymbols then
                True

            else if List.member nt unproductiveSymbols then
                False

            else
                let
                    production =
                        List.filter (.fromSymbol >> (==) nt) glc.productions
                            |> List.head
                in
                case production of
                    Nothing ->
                        False

                    Just p ->
                        List.any
                            (\b ->
                                not (List.member item b)
                                    && List.all
                                        (productivesHelp glc
                                            ( productiveSymbols
                                            , unproductiveSymbols
                                            )
                                        )
                                        b
                            )
                            p.bodies


reassignTerminals : ContextFreeGrammar -> ContextFreeGrammar
reassignTerminals glc =
    let
        allTerminals : List TerminalSymbol
        allTerminals =
            List.concatMap (.bodies >> List.concat) glc.productions
                |> Utils.removeDuplicates
                |> List.filterMap
                    (\i ->
                        case i of
                            Terminal x ->
                                Just x

                            NonTerminal _ ->
                                Nothing
                    )
    in
    { glc
        | terminals =
            List.filter (\t -> List.member t allTerminals) glc.terminals
    }



-- CHOMSKY
-- TESTS (TODO - REMOVE)


test0 : ContextFreeGrammar
test0 =
    { initialSymbol = "S"
    , nonTerminals = [ "S", "B", "D" ]
    , productions =
        [ { bodies =
                [ [ Terminal (Single 'b')
                  , Terminal (Single 'c')
                  , NonTerminal "D"
                  ]
                , [ NonTerminal "B"
                  , Terminal (Single 'c')
                  , Terminal (Single 'd')
                  ]
                ]
          , fromSymbol = "S"
          }
        , { bodies =
                [ [ Terminal (Single 'b'), NonTerminal "B" ]
                , [ Terminal (Single 'b') ]
                ]
          , fromSymbol = "B"
          }
        , { bodies =
                [ [ Terminal (Single 'd')
                  , NonTerminal "B"
                  ]
                , [ Terminal (Single 'd') ]
                ]
          , fromSymbol = "D"
          }
        ]
    , terminals = [ Single 'b', Single 'c', Single 'd' ]
    }


sProd : ContextFreeProduction
sProd =
    { bodies =
        [ [ Terminal (Single 'b')
          , Terminal (Single 'c')
          , NonTerminal "D"
          ]
        , [ NonTerminal "B"
          , Terminal (Single 'c')
          , Terminal (Single 'd')
          ]
        ]
    , fromSymbol = "S"
    }


test1 : ContextFreeGrammar
test1 =
    { initialSymbol = "S"
    , nonTerminals = [ "S", "B", "D", "E" ]
    , productions =
        [ { bodies =
                [ [ Terminal (Single 'b'), Terminal (Single 'c'), NonTerminal "D" ]
                , [ NonTerminal "B", Terminal (Single 'c'), Terminal (Single 'd') ]
                ]
          , fromSymbol = "S"
          }
        , { bodies =
                [ [ Terminal (Single 'b'), NonTerminal "B" ]
                , [ Terminal (Single 'b') ]
                ]
          , fromSymbol = "B"
          }
        , { bodies =
                [ [ Terminal (Single 'd'), NonTerminal "B" ]
                , [ Terminal (Single 'd') ]
                ]
          , fromSymbol = "D"
          }
        , { bodies =
                [ [ Terminal (Single 'c') ]
                , [ Terminal (Single 'b') ]
                ]
          , fromSymbol = "E"
          }
        ]
    , terminals = [ Single 'b', Single 'c', Single 'd' ]
    }


test2 : ContextFreeGrammar
test2 =
    { initialSymbol = "S"
    , nonTerminals = [ "S", "A", "B", "C" ]
    , productions =
        [ { bodies =
                [ [ NonTerminal "A", NonTerminal "B", NonTerminal "B" ]
                , [ NonTerminal "C", NonTerminal "A", NonTerminal "C" ]
                ]
          , fromSymbol = "S"
          }
        , { bodies = [ [ Terminal (Single 'a') ] ], fromSymbol = "A" }
        , { bodies =
                [ [ NonTerminal "B", Terminal (Single 'c') ]
                , [ NonTerminal "A", NonTerminal "B", NonTerminal "B" ]
                ]
          , fromSymbol = "B"
          }
        , { bodies =
                [ [ Terminal (Single 'b'), NonTerminal "B" ]
                , [ Terminal (Single 'a') ]
                ]
          , fromSymbol = "C"
          }
        ]
    , terminals = [ Single 'a', Single 'c', Single 'b' ]
    }
