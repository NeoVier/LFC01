{-
   Models/Grammars.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the definitions of everything related to Grammars
-}


module Models.Grammars exposing (..)

import Models.Alphabet as Alphabet



-- General Grammar type


type Grammar
    = Regular RegularGrammar
    | ContextFree ContextFreeGrammar



-- A Context-Free Grammar is a 4-tuple with a list of non terminal symbols,
-- a list of terminal symbols, a list of productions and an initial symbol.


type alias ContextFreeGrammar =
    { nonTerminals : List NonTerminalSymbol
    , terminals : List TerminalSymbol
    , productions : List ContextFreeProduction
    , initialSymbol : NonTerminalSymbol
    }



-- A context-free production has an origin symbol and a list of context-free
-- production bodies


type alias ContextFreeProduction =
    { fromSymbol : NonTerminalSymbol
    , productions : List ContextFreeProductionBody
    }



-- A context-free production body is a list of terminal/nonterminal symbols,
-- in any order. If the production body is epsilon, the list is empty


type alias ContextFreeProductionBody =
    List ContextFreeProductionItem



-- A production item can either be a terminal symbol or a non terminal symbol


type ContextFreeProductionItem
    = Terminal TerminalSymbol
    | NonTerminal NonTerminalSymbol



-- A Regular Grammar is a 4-tuple with a list of non terminal symbols,
-- a list of terminal symbols, a list of productions and an initial symbol.
-- Plus a flag that determines if it accepts the empty word


type alias RegularGrammar =
    { nonTerminals : List NonTerminalSymbol
    , terminals : List TerminalSymbol
    , productions : List Production
    , initialSymbol : NonTerminalSymbol
    , acceptsEmpty : Bool
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
