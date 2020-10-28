{-
   View/View.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the main view function, along with a few helper functions
-}


module View.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Automata as Automata
import Operations.SentenceValidation as Validation
import Types.Types as Types
import Utils.Utils as Utils
import View.Automata as VAutomata
import View.Styles as Styles



-- The main view function. Defines a title and the viewing areas


view : Types.Model -> Html Types.Msg
view model =
    div Styles.globalStyles
        [ h1 Styles.titleStyles
            [ text
                "Linguagens Formais e Compiladores - Henrique da Cunha Buss"
            ]
        , div
            Styles.mainAreaStyles
            [ viewLeftPanel model
            , viewCenterPanel model
            , viewRightPanel model
            ]
        ]



-- Defines the look of the left panel


viewLeftPanel : Types.Model -> Html Types.Msg
viewLeftPanel model =
    div Styles.leftPanelStyles
        [ h3 Styles.currentAutomatonTitleStyles [ text "Histórico" ]
        , historyView model
        ]



-- Defines the look of the history


historyView : Types.Model -> Html Types.Msg
historyView model =
    let
        label automaton =
            case automaton of
                Automata.FiniteDeterministic _ ->
                    "AFD"

                Automata.FiniteNonDeterministic _ ->
                    "AFND"
    in
    div Styles.historyViewStyles
        (List.map
            (\automaton ->
                div Styles.historyViewRowStyles
                    [ button
                        (Styles.historyViewDeleteStyles
                            ++ [ onClick (Types.RemoveAutomaton automaton) ]
                        )
                        [ text "Remover" ]
                    , button
                        (Styles.historyViewItemStyles
                            ++ [ onClick
                                    (Types.SetAutomaton automaton)
                               ]
                        )
                        [ text (label automaton) ]
                    ]
            )
            model.automataHistory
        )



-- Defines the view of the center panel


viewCenterPanel : Types.Model -> Html Types.Msg
viewCenterPanel model =
    div Styles.currentAutomatonStyles
        [ h3 Styles.currentAutomatonTitleStyles
            [ text "Autômato atual" ]
        , viewSentenceInput model
        , VAutomata.viewCurrentAutomaton model
        ]



-- Defines the view of the sentence input area


viewSentenceInput : Types.Model -> Html Types.Msg
viewSentenceInput model =
    case model.currentAutomaton of
        Err _ ->
            text ""

        Ok automaton ->
            let
                validated =
                    Validation.validateSentence automaton
                        model.currentSentence
            in
            div Styles.sentenceInputStyles
                [ input
                    (Styles.sentenceInputStyles
                        ++ [ placeholder "Insira uma sentença para ser validada"
                           , value model.currentSentence
                           , onInput Types.SetSentence
                           , style "width" "100%"
                           ]
                    )
                    []
                , case validated of
                    Ok _ ->
                        h3 Styles.validSentenceStyles [ text "Sentença Válida" ]

                    Err msg ->
                        h3 Styles.invalidSentenceStyles [ text msg ]
                ]



-- Defines the view of the right panel


viewRightPanel : Types.Model -> Html Types.Msg
viewRightPanel model =
    div Styles.rightPanelStyles
        [ h3 Styles.currentAutomatonTitleStyles
            [ text "Controles" ]
        , div
            Styles.rightPanelControlContainerStyles
            ([ button
                (onClick Types.AFDRequested
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Carregar autômato finito determinístico" ]
             , button
                (onClick Types.AFNDRequested
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Carregar autômato finito não-determinístico" ]
             ]
                ++ [ convertButton model ]
                ++ operationsButtons model
                ++ [ minimizeButton model ]
            )
        ]


convertButton : Types.Model -> Html Types.Msg
convertButton model =
    case model.currentAutomaton of
        Ok (Automata.FiniteNonDeterministic _) ->
            button
                (onClick Types.ConvertAFNDToAFD
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Converter AFND para AFD" ]

        otherwise ->
            text ""


operationsButtons : Types.Model -> List (Html Types.Msg)
operationsButtons model =
    if Utils.firstTwoAreAFDs model.automataHistory then
        [ button (onClick Types.DoUnion :: Styles.rightPanelButtonStyles)
            [ text "Fazer união nos últimos dois autômatos" ]
        , button (onClick Types.DoIntersection :: Styles.rightPanelButtonStyles)
            [ text "Fazer interseção nos últimos dois autômatos" ]
        ]

    else
        [ text "" ]


minimizeButton : Types.Model -> Html Types.Msg
minimizeButton model =
    case model.currentAutomaton of
        Ok (Automata.FiniteDeterministic _) ->
            button (onClick Types.Minimize :: Styles.rightPanelButtonStyles)
                [ text "Minimizar AFD" ]

        otherwise ->
            text ""
