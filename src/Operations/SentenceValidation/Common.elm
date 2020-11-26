{-
   Operations/SentenceValidation/Common.elm
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains common useful functions for sentence validation
-}


module Operations.SentenceValidation.Common exposing
    ( allValidSymbols
    , equivalentSymbols
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


equivalentSymbols : Alphabet.Symbol -> Alphabet.Symbol -> Bool
equivalentSymbols s1 s2 =
    case s1 of
        Alphabet.Single single1 ->
            List.member single1 (symbolItems s2)

        Alphabet.Group _ ->
            case s2 of
                Alphabet.Single single2 ->
                    List.member single2 (symbolItems s1)

                Alphabet.Group _ ->
                    List.any (\s -> List.member s (symbolItems s1))
                        (symbolItems s2)
