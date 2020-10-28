{-
   Operations/Minimization.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to minimize automata
-}


module Operations.Minimization exposing (..)

import Models.Automata as Automata



-- Minimize an AFD


minimizeAFD : Automata.AFD -> Automata.AFD
minimizeAFD afd =
    afd



-- Filter reachable states (can be reached from starting state)
-- Filter alive states (can reach a final state)
-- Do classes
