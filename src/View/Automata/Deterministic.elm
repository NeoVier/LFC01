{-
   View/Automata/Deterministic.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to view deterministic automata
-}


module View.Automata.Deterministic exposing (viewAFD)

import Html exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils
import View.Automata.Common as VC
import View.Styles exposing (..)



-- Given an AFD, return a table that represents it


viewAFD : Automata.AFD -> Html msg
viewAFD afd =
    table tableStyles
        (VC.viewAutomataHeader (Alphabet.Deterministic afd.alphabet)
            :: getAutomatonRows afd
        )



-- Given an AFD, return a list of rows that represent the states and transitions


getAutomatonRows : Automata.AFD -> List (Html msg)
getAutomatonRows afd =
    List.map (\state -> getStateRow afd state) afd.states
        |> List.map
            (\row ->
                tr tableRowStyles
                    (List.map
                        (\entry -> td tableItemStyles [ text entry ])
                        row
                    )
            )



-- Given an AFD and a State, return the row that represents the State


getStateRow : Automata.AFD -> State.State -> List String
getStateRow afd prevState =
    case prevState of
        State.Dead ->
            [ "" ]

        State.Valid label ->
            let
                isInitial =
                    prevState == afd.initialState

                isFinal =
                    List.member prevState afd.finalStates

                prefix =
                    if isInitial && isFinal then
                        "->* "

                    else if isInitial then
                        "-> "

                    else if isFinal then
                        "* "

                    else
                        ""

                transitions =
                    Utils.getFlatOutTransitionsDeterministic
                        afd
                        prevState
                        |> Utils.sortTransitionsDeterministic
            in
            [ prefix
                ++ label
            ]
                ++ List.map (\transition -> viewFlatDeterministicTransition transition)
                    transitions



-- Helper function to convert the nextState of a Transition to a String


viewFlatDeterministicTransition :
    Transition.DeterministicTransition
    -> String
viewFlatDeterministicTransition transition =
    case transition.nextState of
        State.Dead ->
            "-"

        State.Valid nextLabel ->
            nextLabel
