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



{- Shorthand for the kind of Dict used. -}


type alias ItemInfoDict =
    Dict NonTerminalSymbol ItemInfo



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
                Err "TODO"

    else
        Err "Existem símbolos inválidos"



{- Adjust a GLC to validate sentences -}


glcToValidate : ContextFreeGrammar -> Maybe ContextFreeGrammar
glcToValidate glc =
    eliminateLeftRecursion glc |> Result.andThen factorGLC |> Result.toMaybe



{- Constructs the entire ItemInfoDict, populating every field -}


itemInfoDict : ContextFreeGrammar -> ItemInfoDict
itemInfoDict glc =
    -- TODO - Multiple follow passes?
    assignNullables glc |> assignFirsts glc |> assignFollows glc



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



-- TESTS (TODO - REMOVE)


test0 : ContextFreeGrammar
test0 =
    { initialSymbol = "P"
    , nonTerminals = [ "P", "K", "V", "F", "C" ]
    , productions =
        [ { bodies = [ [ NonTerminal "K", NonTerminal "V", NonTerminal "C" ] ]
          , fromSymbol = "P"
          }
        , { bodies = [ [ Terminal (Alphabet.Single 'c'), NonTerminal "K" ], [] ]
          , fromSymbol = "K"
          }
        , { bodies =
                [ [ Terminal (Alphabet.Single 'v')
                  , NonTerminal "V"
                  ]
                , [ NonTerminal "F" ]
                ]
          , fromSymbol = "V"
          }
        , { bodies =
                [ [ Terminal (Alphabet.Single 'f')
                  , NonTerminal "P"
                  , Terminal (Alphabet.Single 's')
                  , NonTerminal "F"
                  ]
                , []
                ]
          , fromSymbol = "F"
          }
        , { bodies =
                [ [ Terminal (Alphabet.Single 'b')
                  , NonTerminal "V"
                  , NonTerminal "C"
                  , Terminal (Alphabet.Single 'e')
                  ]
                , [ Terminal (Alphabet.Single 'm')
                  , Terminal (Alphabet.Single 's')
                  , NonTerminal "C"
                  ]
                , []
                ]
          , fromSymbol = "C"
          }
        ]
    , terminals =
        [ Alphabet.Single 'c'
        , Alphabet.Single 'v'
        , Alphabet.Single 'f'
        , Alphabet.Single 's'
        , Alphabet.Single 'b'
        , Alphabet.Single 'e'
        , Alphabet.Single 'm'
        ]
    }
