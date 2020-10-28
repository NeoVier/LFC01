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
