{-
   View/Grammars.elm
   Author: Henrique da Cunha Buss
   Creation: November/2020
   This file contains functions to view general grammars
-}


module View.Grammars exposing (..)

import Html exposing (..)
import Models.Grammars as Grammars
import Types.Types as Types
import View.Grammars.ContextFree as VGLC
import View.Grammars.Regular as VGR



{- General view function, that routes to the appropriate function -}


viewCurrentGrammar : Grammars.Grammar -> Html Types.Msg
viewCurrentGrammar grammar =
    case grammar of
        Grammars.Regular gr ->
            VGR.viewGR gr

        Grammars.ContextFree glc ->
            VGLC.viewGLC glc
