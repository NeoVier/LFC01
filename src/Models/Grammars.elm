{-
   Models/Grammars.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the definitions of everything related to Grammars
-}
-- TODO accept epsilon


module Models.Grammars exposing (..)

import Models.Alphabet as Alphabet



-- A Regular Grammar is a 4-tuple with a list of non terminal symbols,
-- a list of terminal symbols, a list of productions and an initial symbol


type alias Grammar =
    { nonTerminals : List NonTerminalSymbol
    , terminals : List TerminalSymbol
    , productions : List Production
    , initialSymbol : NonTerminalSymbol
    }



-- A production has an origin symbol and a list of production bodies


type alias Production =
    { fromSymbol : NonTerminalSymbol
    , productions : List ProductionBody
    }



-- A production body has a symbol it consumes, and it may go to another non
-- terminal symbol


type alias ProductionBody =
    { consumed : TerminalSymbol
    , toSymbol : Maybe NonTerminalSymbol
    }



-- A non terminal symbol is just a regular symbol (by convention, it's upper
-- case)


type alias NonTerminalSymbol =
    String



-- A terminal symbol is just a regular symbol (by convention, it's lower case)


type alias TerminalSymbol =
    Alphabet.Symbol
