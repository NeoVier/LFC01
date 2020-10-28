{-
   Operations/Basics.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to execute basic operations on AFDs, such as
   union and intersection
-}


module Operations.Basics exposing (..)

import Models.Automata as Automata



-- Performs the union operation with two AFDs


union : Automata.AFD -> Automata.AFD -> Automata.AFD
union afd1 afd2 =
    afd1



-- Performs the intersection operation with two AFDs


intersection : Automata.AFD -> Automata.AFD -> Automata.AFD
intersection afd1 afd2 =
    afd1
