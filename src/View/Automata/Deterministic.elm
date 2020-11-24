{-
   View/Automata/Deterministic.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains functions to view deterministic automata
-}


module View.Automata.Deterministic exposing (viewAFD)

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



{- Given an AFD, return a table that represents it -}


viewAFD : Automata.AFD -> Html Types.Msg
viewAFD afd =
    table tableStyles
        (VC.viewAutomataHeader (Alphabet.Deterministic afd.alphabet)
            :: getAutomatonRows afd
        )



{- Given an AFD, return a list of rows that represent the states and
   transitions
-}


getAutomatonRows : Automata.AFD -> List (Html Types.Msg)
getAutomatonRows afd =
    List.map (\state -> getStateRow afd state) afd.states



{- Given an AFD and a State, return the row that represents the State -}


getStateRow : Automata.AFD -> State.State -> Html Types.Msg
getStateRow afd prevState =
    let
        isInitial =
            prevState == afd.initialState

        isFinal =
            List.member prevState afd.finalStates

        prefix =
            if isInitial && isFinal then
                "->*"

            else if isInitial then
                "->"

            else if isFinal then
                "*"

            else
                ""

        prefixSelect =
            select
                [ onInput
                    (\new ->
                        case new of
                            "->*" ->
                                { afd
                                    | initialState = prevState
                                    , finalStates =
                                        afd.finalStates
                                            ++ [ prevState ]
                                            |> Utils.removeDuplicates
                                }
                                    |> afdToGeneral
                                    |> Types.UpdateCurrent

                            "->" ->
                                { afd
                                    | initialState = prevState
                                    , finalStates =
                                        List.filter ((/=) prevState)
                                            afd.finalStates
                                }
                                    |> afdToGeneral
                                    |> Types.UpdateCurrent

                            "*" ->
                                { afd
                                    | finalStates =
                                        afd.finalStates
                                            ++ [ prevState ]
                                            |> Utils.removeDuplicates
                                }
                                    |> afdToGeneral
                                    |> Types.UpdateCurrent

                            _ ->
                                { afd
                                    | finalStates =
                                        List.filter ((/=) prevState)
                                            afd.finalStates
                                }
                                    |> afdToGeneral
                                    |> Types.UpdateCurrent
                    )
                ]
                (List.map
                    (\t ->
                        option [ value t, selected (t == prefix) ] [ text t ]
                    )
                    (if prevState == afd.initialState then
                        [ "->*", "->" ]

                     else
                        [ "->*", "->", "*", "" ]
                    )
                )

        transitions =
            Utils.getFlatOutTransitionsDeterministic
                afd
                prevState
                |> Utils.sortTransitionsDeterministic
    in
    tr tableRowStyles <|
        td tableItemStyles
            [ button
                [ disabled isInitial
                , onClick
                    ({ afd
                        | states =
                            List.filter ((/=) prevState) afd.states
                        , finalStates =
                            List.filter ((/=) prevState)
                                afd.finalStates
                        , transitions =
                            List.filterMap
                                (\transition ->
                                    if transition.nextState == prevState then
                                        Just
                                            { transition
                                                | nextState = State.Dead
                                            }

                                    else if
                                        transition.prevState
                                            == prevState
                                    then
                                        Nothing

                                    else
                                        Just transition
                                )
                                afd.transitions
                     }
                        |> afdToGeneral
                        |> Types.UpdateCurrent
                    )
                ]
                [ text "Remover estado" ]
            ]
            :: td tableItemStyles [ prefixSelect, text <| stateToString prevState ]
            :: List.map
                (\transition ->
                    td tableItemStyles
                        [ viewFlatDeterministicTransition transition afd ]
                )
                transitions



{- Turn a deterministic transition into HTML -}


viewFlatDeterministicTransition :
    Transition.DeterministicTransition
    -> Automata.AFD
    -> Html Types.Msg
viewFlatDeterministicTransition transition afd =
    select
        [ onInput
            (\new ->
                { afd
                    | transitions =
                        Utils.replaceBy transition
                            { transition | nextState = stringToState new }
                            afd.transitions
                }
                    |> Automata.FiniteDeterministic
                    |> Models.Automaton
                    |> Types.UpdateCurrent
            )
        ]
        (List.map
            (\state ->
                option
                    [ value (stateToString state)
                    , selected (state == transition.nextState)
                    ]
                    [ text (stateToString state) ]
            )
            (afd.states ++ [ State.Dead ])
        )



{- Turn an AFD into a general model -}


afdToGeneral : Automata.AFD -> Models.General
afdToGeneral =
    Automata.FiniteDeterministic >> Models.Automaton



{- Turn a state into a string -}


stateToString : State.State -> String
stateToString state =
    case state of
        State.Dead ->
            "-"

        State.Valid label ->
            label



{- Create a state out of a string -}


stringToState : String -> State.State
stringToState label =
    if label == "-" then
        State.Dead

    else
        State.Valid label
