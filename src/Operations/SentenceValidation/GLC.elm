{-
   Operations/SentenceValidation/GLC
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains functions to validate sentences using a GLC
-}
-- module Operations.SentenceValidation.GLC exposing (validateSentence)


module Operations.SentenceValidation.GLC exposing (..)

-- import GenericDict as Dict exposing (Dict)

import Dict exposing (Dict)
import Models.Alphabet as Alphabet
import Models.Grammars exposing (..)
import Operations.GLC exposing (eliminateLeftRecursion, factorGLC)
import Operations.SentenceValidation.Common exposing (..)
import Utils.GLC as GLCUtils
import Utils.Utils as Utils



{- Contains info for an item: if it's nullable, it's first and follow values.
   If nullable, first contains &
-}


type alias ItemInfo =
    { nullable : Bool
    , first : List TerminalSymbol
    , follow : List TerminalSymbol
    }



{- Shorthand for the kind of Dict used -}


type alias ItemInfoDict =
    Dict NonTerminalSymbol ItemInfo


type alias AnalysisTable =
    List AnalysisTableEntry


type alias AnalysisTableEntry =
    { terminal : TerminalSymbol
    , nonTerminal : NonTerminalSymbol
    , value : ContextFreeProductionBody
    }



{- Terminal symbol that represents the end of a sentence -}


endOfSentence : TerminalSymbol
endOfSentence =
    Alphabet.Single '$'



{- Validate a sentence using a GLC. Uses LL(1) -}


validateSentence : ContextFreeGrammar -> String -> Result String Bool
validateSentence inputGlc sentence =
    let
        fixedGlc =
            glcToValidate inputGlc
    in
    if allValidSymbols inputGlc.terminals sentence then
        case fixedGlc of
            Nothing ->
                Err "LL(1) não pode ser usado (gramática não pôde ser fatorada ou ter sua recursão à esquerda eliminada)"

            Just glc ->
                let
                    info =
                        itemInfoDict glc

                    anyIntersections =
                        List.any
                            (\v ->
                                if v.nullable then
                                    let
                                        firstPlusFollow =
                                            v.first ++ v.follow
                                    in
                                    Utils.removeDuplicates firstPlusFollow
                                        /= firstPlusFollow

                                else
                                    False
                            )
                            (Dict.values info)
                in
                if anyIntersections then
                    Err "LL(1) não pode ser usado (existe interseção entre um follow e um first)"

                else
                    let
                        table =
                            analysisTable glc info
                    in
                    Ok (validate table glc.initialSymbol sentence)

    else
        Err "Existem símbolos inválidos"


validate : AnalysisTable -> NonTerminalSymbol -> String -> Bool
validate table startingPoint input =
    let
        inputAsSymbols =
            String.toList input
                |> List.map Alphabet.Single
    in
    validateHelp table
        [ NonTerminal startingPoint, Terminal endOfSentence ]
        (inputAsSymbols ++ [ endOfSentence ])


validateHelp :
    AnalysisTable
    -> List ContextFreeProductionItem
    -> List TerminalSymbol
    -> Bool
validateHelp table stack input =
    case List.head input of
        -- If there's no input, we're done
        Nothing ->
            stack
                == []
                && input
                == []

        Just inputHead ->
            case List.head stack of
                -- If there's nothing at the stack, but there's still some input
                -- left, it's invalid
                Nothing ->
                    False

                Just (Terminal x) ->
                    -- If the top of the stack is equal to the input head,
                    -- we consume the input
                    if equivalentSymbols x inputHead then
                        validateHelp table
                            (List.drop 1 stack)
                            (List.drop 1 input)
                        -- If the top of the stack is not equal to the input
                        -- head, the input is invalid

                    else
                        False

                Just (NonTerminal x) ->
                    case getValue table inputHead x of
                        -- We reached an invalid state at the table
                        Nothing ->
                            False

                        -- Consume the stack and recurse
                        Just v ->
                            validateHelp table (v ++ List.drop 1 stack) input



{- Adjust a GLC to validate sentences -}


glcToValidate : ContextFreeGrammar -> Maybe ContextFreeGrammar
glcToValidate glc =
    eliminateLeftRecursion glc |> Result.andThen factorGLC |> Result.toMaybe



{- Create the analysis table for a grammar with an item info dict -}


analysisTable : ContextFreeGrammar -> ItemInfoDict -> AnalysisTable
analysisTable glc info =
    List.foldl
        (\prod accTable ->
            List.foldl
                (\body bodyTable ->
                    case List.head body of
                        -- Epsilon -
                        Nothing ->
                            addEpsilonBody bodyTable info prod.fromSymbol body

                        Just (Terminal x) ->
                            addTerminalBody bodyTable prod.fromSymbol x body

                        Just (NonTerminal _) ->
                            addBodyBody bodyTable info prod.fromSymbol body
                )
                accTable
                prod.bodies
        )
        []
        glc.productions



{- Add body to everything in
   [prod.fromSymbol, follow(prod.fromSymbol)]
-}


addEpsilonBody :
    AnalysisTable
    -> ItemInfoDict
    -> NonTerminalSymbol
    -> ContextFreeProductionBody
    -> AnalysisTable
addEpsilonBody table info nt body =
    let
        follows =
            Dict.get nt info |> Maybe.map .follow |> Maybe.withDefault []
    in
    List.foldl
        (\t accTable ->
            Utils.insertUnique
                { nonTerminal = nt
                , terminal = t
                , value = body
                }
                accTable
        )
        table
        follows



{- Add body to [prod.fromSymbol, x] -}


addTerminalBody :
    AnalysisTable
    -> NonTerminalSymbol
    -> TerminalSymbol
    -> ContextFreeProductionBody
    -> AnalysisTable
addTerminalBody table nt t body =
    Utils.insertUnique
        { nonTerminal = nt
        , terminal = t
        , value = body
        }
        table



{- Add body to everything in [prod.fromSymbol, first(body)]

   If body is nullable, add body to everything in
   [prod.fromSymbol, follow(prod.fromSymbol)]
-}


addBodyBody :
    AnalysisTable
    -> ItemInfoDict
    -> NonTerminalSymbol
    -> ContextFreeProductionBody
    -> AnalysisTable
addBodyBody table info nt body =
    let
        initial =
            List.foldl
                (\t accTable ->
                    Utils.insertUnique
                        { nonTerminal = nt
                        , terminal = t
                        , value = body
                        }
                        accTable
                )
                table
                (firstFromBody info body)
    in
    if isBodyNullable info body then
        let
            follows =
                Dict.get nt info |> Maybe.map .follow |> Maybe.withDefault []
        in
        List.foldl
            (\t accTable ->
                Utils.insertUnique
                    { nonTerminal = nt
                    , terminal = t
                    , value = body
                    }
                    accTable
            )
            initial
            follows

    else
        initial



{- Determines if a body is nullable -}


isBodyNullable : ItemInfoDict -> ContextFreeProductionBody -> Bool
isBodyNullable info =
    List.all
        (\s ->
            case s of
                Terminal _ ->
                    False

                NonTerminal x ->
                    Dict.get x info
                        |> Maybe.map .nullable
                        |> Maybe.withDefault False
        )



{- Get the first for the body -}


firstFromBody : ItemInfoDict -> ContextFreeProductionBody -> List TerminalSymbol
firstFromBody info body =
    List.foldl
        (\s ( continue, accFirst ) ->
            if not continue then
                ( continue, accFirst )

            else
                case s of
                    Terminal x ->
                        ( False, Utils.insertUnique x accFirst )

                    NonTerminal x ->
                        let
                            isNullable =
                                Dict.get x info
                                    |> Maybe.map .nullable
                                    |> Maybe.withDefault False

                            firsts =
                                Dict.get x info
                                    |> Maybe.map .first
                                    |> Maybe.withDefault []
                        in
                        ( isNullable, Utils.concatUnique accFirst firsts )
        )
        ( True, [] )
        body
        |> Tuple.second



{- Get an entry from an analysis table -}


getEntry :
    AnalysisTable
    -> TerminalSymbol
    -> NonTerminalSymbol
    -> Maybe AnalysisTableEntry
getEntry table t nt =
    List.filter (\e -> equivalentSymbols e.terminal t && e.nonTerminal == nt)
        table
        |> List.head



{- Get a value from an analysis table -}


getValue :
    AnalysisTable
    -> TerminalSymbol
    -> NonTerminalSymbol
    -> Maybe ContextFreeProductionBody
getValue table t =
    getEntry table t >> Maybe.map .value



{- Constructs the entire ItemInfoDict, populating every field -}


itemInfoDict : ContextFreeGrammar -> ItemInfoDict
itemInfoDict glc =
    assignNullables glc |> assignFirsts glc |> assignFollows glc



{- Insert a list of terminal symbols into first -}


insertIntoFirst :
    ItemInfoDict
    -> NonTerminalSymbol
    -> List TerminalSymbol
    -> ItemInfoDict
insertIntoFirst info nt ts =
    Dict.update nt
        (Maybe.map (\v -> { v | first = Utils.concatUnique v.first ts }))
        info



{- Insert a list of terminal symbols into follow -}


insertIntoFollow :
    ItemInfoDict
    -> NonTerminalSymbol
    -> List TerminalSymbol
    -> ItemInfoDict
insertIntoFollow info nt ts =
    Dict.update nt
        (Maybe.map
            (\v ->
                { v | follow = Utils.concatUnique v.follow ts }
            )
        )
        info



{- Starts an ItemInfoDict, assigning the nullable values -}


assignNullables : ContextFreeGrammar -> ItemInfoDict
assignNullables glc =
    let
        nullables =
            GLCUtils.nullables glc
    in
    List.map
        (\nt ->
            ( nt
            , { nullable = List.member nt nullables
              , first = []
              , follow = []
              }
            )
        )
        glc.nonTerminals
        |> Dict.fromList



{- Assign all the first fields of an item info dict -}


assignFirsts : ContextFreeGrammar -> ItemInfoDict -> ItemInfoDict
assignFirsts glc info =
    List.foldl (first glc) info glc.nonTerminals



{- Get first using a dict -}


first :
    ContextFreeGrammar
    -> NonTerminalSymbol
    -> ItemInfoDict
    -> ItemInfoDict
first glc nt info =
    GLCUtils.productionFromSymbol glc nt
        |> Maybe.map
            (.bodies
                >> List.foldl
                    (\body accInfo ->
                        case body of
                            -- If it's just a single terminal, add it to first
                            (Terminal x) :: _ ->
                                insertIntoFirst accInfo nt [ x ]

                            _ ->
                                iterateBodyForFirst glc accInfo nt body
                    )
                    info
            )
        |> Maybe.withDefault info



{- Iterate over a body and insert it into first -}


iterateBodyForFirst :
    ContextFreeGrammar
    -> ItemInfoDict
    -> NonTerminalSymbol
    -> ContextFreeProductionBody
    -> ItemInfoDict
iterateBodyForFirst glc info fromSymbol body =
    List.foldl
        (\symbol ( continue, accInfo ) ->
            if not continue then
                ( continue, accInfo )

            else
                case symbol of
                    Terminal x ->
                        ( False, insertIntoFirst accInfo fromSymbol [ x ] )

                    NonTerminal x ->
                        let
                            isNullable =
                                Dict.get x accInfo
                                    |> Maybe.map .nullable
                                    |> Maybe.withDefault False

                            result =
                                first glc x accInfo

                            xFirst =
                                Dict.get x result
                                    |> Maybe.map .first
                                    |> Maybe.withDefault []
                        in
                        ( isNullable, insertIntoFirst result fromSymbol xFirst )
        )
        ( True, info )
        body
        |> Tuple.second



{- Assign all the follow fields of an item info dict -}


assignFollows : ContextFreeGrammar -> ItemInfoDict -> ItemInfoDict
assignFollows glc info =
    insertIntoFollow info glc.initialSymbol [ endOfSentence ]
        |> followFirstPass glc
        |> followSecondPass glc



{- First step for getting follows -}


followFirstPass : ContextFreeGrammar -> ItemInfoDict -> ItemInfoDict
followFirstPass glc info =
    List.foldl (followFirstPassHelp glc) info glc.nonTerminals



{- Helper function -}


followFirstPassHelp :
    ContextFreeGrammar
    -> NonTerminalSymbol
    -> ItemInfoDict
    -> ItemInfoDict
followFirstPassHelp glc nt info =
    let
        bodiesAfter =
            List.concatMap .bodies glc.productions
                |> List.filter (List.member (NonTerminal nt))
                |> List.map (Utils.dropUntil (NonTerminal nt))
    in
    List.foldl
        (\body accInfo ->
            List.foldl
                (\symbol ( continue, bodyInfo ) ->
                    if not continue then
                        ( continue, bodyInfo )

                    else
                        case symbol of
                            Terminal x ->
                                ( False, insertIntoFollow bodyInfo nt [ x ] )

                            NonTerminal x ->
                                let
                                    isNullable =
                                        Dict.get x bodyInfo
                                            |> Maybe.map .nullable
                                            |> Maybe.withDefault False

                                    firsts =
                                        Dict.get x bodyInfo
                                            |> Maybe.map .first
                                            |> Maybe.withDefault []
                                in
                                ( isNullable
                                , insertIntoFollow bodyInfo nt firsts
                                )
                )
                ( True, accInfo )
                body
                |> Tuple.second
        )
        info
        bodiesAfter



{- Second step for getting follows -}


followSecondPass : ContextFreeGrammar -> ItemInfoDict -> ItemInfoDict
followSecondPass glc info =
    List.foldl (\p accInfo -> followSecondPassHelp glc p accInfo)
        info
        glc.productions



{- Helper function -}


followSecondPassHelp :
    ContextFreeGrammar
    -> ContextFreeProduction
    -> ItemInfoDict
    -> ItemInfoDict
followSecondPassHelp glc prod info =
    let
        followA =
            Dict.get prod.fromSymbol info
                |> Maybe.map .follow
                |> Maybe.withDefault []
    in
    List.foldl
        (\reversedBody accInfo ->
            List.foldl
                (\symbol ( continue, bodyInfo ) ->
                    if not continue then
                        ( continue, bodyInfo )

                    else
                        case symbol of
                            Terminal _ ->
                                ( False, bodyInfo )

                            NonTerminal x ->
                                let
                                    isNullable =
                                        Dict.get x bodyInfo
                                            |> Maybe.map .nullable
                                            |> Maybe.withDefault False
                                in
                                ( isNullable
                                , insertIntoFollow bodyInfo x followA
                                )
                )
                ( True, accInfo )
                reversedBody
                |> Tuple.second
        )
        info
        (List.map List.reverse prod.bodies)
