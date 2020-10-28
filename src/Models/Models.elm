{-
   Models/Models.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the definition of general models like Automata, Grammars
   and RegExes
-}


module Models.Models exposing (..)

import Models.Automata as Automata
import Models.Grammars as Grammars



-- General type


type General
    = Automaton Automata.Automaton
    | Grammar Grammars.Grammar
