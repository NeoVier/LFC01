{-
   Models/State.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the definition of everything related to States
-}


module Models.State exposing (..)

-- A State can either be the Dead state or a Valid State with a label


type State
    = Valid String
    | Dead
