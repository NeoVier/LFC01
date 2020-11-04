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
    let
        isInitial =
            prevState == afnd.initialState

        isFinal =
            List.member prevState afnd.finalStates

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
                                { afnd
                                    | initialState = prevState
                                    , finalStates =
                                        afnd.finalStates
                                            ++ [ prevState ]
                                            |> Utils.removeDuplicates
                                }
                                    |> afndToGeneral
                                    |> Types.UpdateCurrent

                            "->" ->
                                { afnd
                                    | initialState = prevState
                                    , finalStates =
                                        List.filter ((/=) prevState)
                                            afnd.finalStates
                                }
                                    |> afndToGeneral
                                    |> Types.UpdateCurrent

                            "*" ->
                                { afnd
                                    | finalStates =
                                        afnd.finalStates
                                            ++ [ prevState ]
                                            |> Utils.removeDuplicates
                                }
                                    |> afndToGeneral
                                    |> Types.UpdateCurrent

                            _ ->
                                { afnd
                                    | finalStates =
                                        List.filter
                                            ((/=) prevState)
                                            afnd.finalStates
                                }
                                    |> afndToGeneral
                                    |> Types.UpdateCurrent
                    )
                ]
                (List.map
                    (\t ->
                        option
                            [ value t, selected (t == prefix) ]
                            [ text t ]
                    )
                    (if prevState == afnd.initialState then
                        [ "->*", "->" ]

                     else
                        [ "->*", "->", "*", "" ]
                    )
                )

        transitions =
            Utils.getFlatOutTransitionsNonDeterministic afnd prevState
                |> Utils.sortTransitionsNonDeterministic
                |> Utils.swapFirstAndLast
    in
    tr tableRowStyles
        (td tableItemStyles
            [ button
                [ disabled isInitial
                , onClick
                    ({ afnd
                        | states = List.filter ((/=) prevState) afnd.states
                        , finalStates =
                            List.filter ((/=) prevState) afnd.finalStates
                        , transitions =
                            List.filterMap
                                (\transition ->
                                    if
                                        List.member prevState
                                            transition.nextStates
                                    then
                                        Just
                                            { transition
                                                | nextStates =
                                                    case
                                                        transition.nextStates
                                                            |> List.filter
                                                                ((/=) prevState)
                                                    of
                                                        [] ->
                                                            [ State.Dead ]

                                                        a ->
                                                            a
                                            }

                                    else if
                                        transition.prevState
                                            == prevState
                                    then
                                        Nothing

                                    else
                                        Just transition
                                )
                                afnd.transitions
                     }
                        |> afndToGeneral
                        |> Types.UpdateCurrent
                    )
                ]
                [ text "Remover estado" ]
            ]
            :: td tableItemStyles
                [ prefixSelect
                , text <| stateToString prevState
                ]
            :: List.map
                (\transition ->
                    viewFlatNonDeterministicTransition transition afnd
                        |> td tableItemStyles
                )
                transitions
        )



-- Helper function to convert the nextState of a Transition to a String


viewFlatNonDeterministicTransition :
    Transition.NonDeterministicTransition
    -> Automata.AFND
    -> List (Html Types.Msg)
viewFlatNonDeterministicTransition transition afnd =
    List.map
        (\state ->
            select
                [ onInput
                    (\new ->
                        { afnd
                            | transitions =
                                Utils.replaceBy transition
                                    { transition
                                        | nextStates =
                                            case
                                                Utils.replaceBy
                                                    state
                                                    (stringToState new)
                                                    transition.nextStates
                                                    |> Utils.removeDuplicates
                                                    |> List.filter
                                                        ((/=) State.Dead)
                                            of
                                                [] ->
                                                    [ State.Dead ]

                                                a ->
                                                    a
                                    }
                                    afnd.transitions
                        }
                            |> afndToGeneral
                            |> Types.UpdateCurrent
                    )
                ]
                (List.filterMap
                    (\s ->
                        if
                            List.member s transition.nextStates
                                && s
                                /= state
                        then
                            Nothing

                        else
                            Just <|
                                option
                                    [ value (stateToString s)
                                    , selected (s == state)
                                    ]
                                    [ text (stateToString s) ]
                    )
                    (afnd.states ++ [ State.Dead ])
                )
        )
        transition.nextStates
        ++ [ select
                [ onInput
                    (\new ->
                        { afnd
                            | transitions =
                                Utils.replaceBy transition
                                    { transition
                                        | nextStates =
                                            transition.nextStates
                                                ++ [ stringToState new ]
                                                |> List.filter ((/=) State.Dead)
                                    }
                                    afnd.transitions
                        }
                            |> afndToGeneral
                            |> Types.UpdateCurrent
                    )
                ]
                (option [ value "+", selected True ] [ text "+" ]
                    :: List.filterMap
                        (\state ->
                            if List.member state transition.nextStates then
                                Nothing

                            else
                                Just <|
                                    option [ value (stateToString state) ]
                                        [ text (stateToString state) ]
                        )
                        afnd.states
                )
           ]


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
