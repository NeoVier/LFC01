module Models.Grammars exposing (..)

import Models.Alphabet as Alphabet


type alias Grammar =
    { nonTerminals : List NonTerminalSymbol
    , terminals : List TerminalSymbol
    , productions : List Production
    , initialSymbol : NonTerminalSymbol
    }


type alias Production =
    { fromSymbol : NonTerminalSymbol
    , productions : List ProductionBody
    }


type alias ProductionBody =
    { consumed : Maybe NonTerminalSymbol
    , toSymbol : Maybe TerminalSymbol
    }


type alias NonTerminalSymbol =
    Alphabet.Symbol


type alias TerminalSymbol =
    Alphabet.Symbol
