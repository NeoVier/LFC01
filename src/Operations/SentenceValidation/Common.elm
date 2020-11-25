{-
   Operations/SentenceValidation/Common.elm
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains common useful functions for sentence validation
-}


module Operations.SentenceValidation.Common exposing
    ( allValidSymbols
    , symbolItems
    )

import Models.Alphabet as Alphabet



{- Checks wheter a string is entirely contained by an alphabet -}


allValidSymbols : Alphabet.DeterministicAlphabet -> String -> Bool
allValidSymbols alphabet sentence =
    let
        alphabetSymbols =
            List.concatMap symbolItems alphabet
    in
    String.toList sentence
        |> List.all (\symbol -> List.member symbol alphabetSymbols)



{- Given a Symbol, get the List of Chars it represents (specially useful for
   Groups)
-}


symbolItems : Alphabet.Symbol -> List Char
symbolItems symbol =
    case symbol of
        Alphabet.Single s ->
            [ s ]

        Alphabet.Group g ->
            List.concatMap innerGroupRange g



{- Given a group element, inform the List of Chars it contains -}


innerGroupRange : ( Char, Char ) -> List Char
innerGroupRange group =
    case group of
        ( a, b ) ->
            List.range (Char.toCode a) (Char.toCode b)
                |> List.map Char.fromCode
