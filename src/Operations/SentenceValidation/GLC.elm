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



{- Starts an ItemInfoDict, assigning the nullable values, and inserting
   end of sentence in the follow of the initial symbol
-}


createItemInfoDict : ContextFreeGrammar -> ItemInfoDict
createItemInfoDict glc =
    let
        nullables =
            GLCUtils.nullables glc
    in
    List.map
        (\nt ->
            ( nt
            , { nullable = List.member nt nullables
              , first = []
              , follow =
                    if nt == glc.initialSymbol then
                        [ endOfSentence ]

                    else
                        []
              }
            )
        )
        glc.nonTerminals
        |> Dict.fromList



{- Assign all the first fields of an item info dict -}


assignFirsts : ContextFreeGrammar -> ItemInfoDict -> ItemInfoDict
assignFirsts glc info =
    List.foldl (first glc) info glc.nonTerminals



-- first glc nt Dict.empty
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
