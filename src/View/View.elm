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
    div Styles.historyViewStyles
        (List.map
            (\automaton ->
                case automaton of
                    Automata.FiniteDeterministic afd ->
                        button
                            (Styles.historyViewItemStyles
                                ++ [ onClick
                                        (Types.SetAutomaton automaton)
                                   ]
                            )
                            [ text "AFD" ]

                    Automata.FiniteNonDeterministic afnd ->
                        button
                            (Styles.historyViewItemStyles
                                ++ [ onClick
                                        (Types.SetAutomaton automaton)
                                   ]
                            )
                            [ text "AFND" ]
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
    let
        validated =
            case model.currentAutomaton of
                Ok automaton ->
                    Validation.validateSentence automaton
                        model.currentSentence

                Err _ ->
                    Err ""
    in
    div []
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
            [ button
                (onClick Types.AFDRequested
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Carregar autômato finito determinístico" ]
            , button
                (onClick Types.AFNDRequested
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Carregar autômato finito não-determinístico" ]
            , case model.currentAutomaton of
                Ok (Automata.FiniteNonDeterministic _) ->
                    button
                        (onClick Types.ConvertAFNDToAFD
                            :: Styles.rightPanelButtonStyles
                        )
                        [ text "Converter AFND para AFD" ]

                otherwise ->
                    text ""
            ]
        ]
