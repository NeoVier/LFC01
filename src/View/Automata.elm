{-
   View/Automata.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to view general automata
-}


module View.Automata exposing (..)

import Html exposing (..)
import Models.Automata as Automata
import Types.Types as Types
import View.Automata.Deterministic as VAFD
import View.Automata.NonDeterministic as VAFND



-- General view function, that routes to the appropriate function


viewCurrentAutomaton : Types.Model -> Html msg
viewCurrentAutomaton model =
    case model.currentAutomaton of
        Err message ->
            h1 [] [ text message ]

        Ok automaton ->
            case automaton of
                Automata.FiniteDeterministic afd ->
                    VAFD.viewAFD afd

                Automata.FiniteNonDeterministic afnd ->
                    VAFND.viewAFND afnd
