{-
   Models/Regex.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the definitions of everything related to Regexes
-}


module Models.Regex exposing (..)

import Models.Alphabet as Alphabet



-- A Regex can either be Epsilon, a Symbol, the Union between two Regexes,
-- the Concatenation between two Regexes, or a Star, Plus or Question node


type Regex
    = Epsilon
    | Symbol Alphabet.Symbol
    | Union Regex Regex
    | Concat Regex Regex
    | Star Regex
    | Plus Regex
    | Question Regex



-- We can have Regexes with Ids


type alias IdRegex =
    ( String, Regex )
