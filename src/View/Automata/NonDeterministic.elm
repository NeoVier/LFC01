{-
   View/Automata/NonDeterministic.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to view non deterministic automata
-}


module View.Automata.NonDeterministic exposing (viewAFND)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.Models as Models
import Models.State as State
import Models.Transition as Transition
import Types.Types as Types
import Utils.Utils as Utils
import View.Automata.Common as VC
import View.Styles exposing (..)



-- Given an AFND, return a table that represents it


viewAFND : Automata.AFND -> Html Types.Msg
viewAFND afnd =
    table tableStyles
        (VC.viewAutomataHeader (Alphabet.NonDeterministic afnd.alphabet)
            :: getAutomatonRows afnd
        )



-- Given an AFND, return a list of rows that represent the states and transitions


getAutomatonRows : Automata.AFND -> List (Html Types.Msg)
getAutomatonRows afnd =
    List.map (\state -> getStateRow afnd state) afnd.states



-- Given an AFND and a State, return the row that represents the State


getStateRow : Automata.AFND -> State.State -> Html Types.Msg
getStateRow afnd prevState =
    case prevState of
        State.Dead ->
            text ""

        State.Valid label ->
            let
                isInitial =
                    prevState == afnd.initialState

                isFinal =
                    List.member prevState afnd.finalStates

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
                    Utils.getFlatOutTransitionsNonDeterministic afnd prevState
                        |> Utils.sortTransitionsNonDeterministic
                        |> Utils.swapFirstAndLast
            in
            tr tableRowStyles
                (td tableItemStyles []
                    :: td tableItemStyles [ text (prefix ++ label) ]
                    :: List.map
                        (\transition ->
                            viewFlatNonDeterministicTransition transition afnd
                        )
                        transitions
                )



-- Helper function to convert the nextState of a Transition to a String


viewFlatNonDeterministicTransition :
    Transition.NonDeterministicTransition
    -> Automata.AFND
    -> Html Types.Msg
viewFlatNonDeterministicTransition transition afnd =
    select []
        (option [] [ text (statesToString transition.nextStates) ]
            :: []
        )



-- if transition.nextStates == [ State.Dead ] then
-- td tableItemStyles [ text "-" ]
-- else
-- td tableItemStyles
-- [ List.map
-- (\state ->
-- case state of
-- State.Dead ->
-- ""
-- State.Valid label ->
-- label
-- )
-- transition.nextStates
-- |> List.filter (\x -> x /= "")
-- |> String.join ", "
-- |> text
-- ]


afndToGeneral : Automata.AFND -> Models.General
afndToGeneral =
    Automata.FiniteNonDeterministic >> Models.Automaton


statesToString : List State.State -> String
statesToString states =
    case states of
        [ State.Dead ] ->
            "-"

        _ ->
            List.filter (\state -> state /= State.Dead) states
                |> List.map stateToString
                |> String.join ", "


stateToString : State.State -> String
stateToString state =
    case state of
        State.Dead ->
            "-"

        State.Valid label ->
            label


stringToState : String -> State.State
stringToState label =
    if label == "-" then
        State.Dead

    else
        State.Valid label
