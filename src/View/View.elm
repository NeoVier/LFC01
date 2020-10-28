module View.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Automata as Automata
import Types.Types as Types
import Utils.SentenceValidation as Validation
import View.Automata as VAutomata
import View.Styles as Styles


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


viewLeftPanel : Types.Model -> Html Types.Msg
viewLeftPanel model =
    div Styles.leftPanelStyles
        [ h3 Styles.currentAutomatonTitleStyles [ text "Histórico" ]
        , historyView model
        ]


viewCenterPanel : Types.Model -> Html Types.Msg
viewCenterPanel model =
    div Styles.currentAutomatonStyles
        [ h3 Styles.currentAutomatonTitleStyles
            [ text "Autômato atual" ]
        , viewSentenceInput model
        , VAutomata.viewCurrentAutomaton model
        ]


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
