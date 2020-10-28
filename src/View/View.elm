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
import Models.Models as Models
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
        label general =
            case general of
                Models.Automaton (Automata.FiniteDeterministic _) ->
                    "AFD"

                Models.Automaton (Automata.FiniteNonDeterministic _) ->
                    "AFND"

                otherwise ->
                    "Error"
    in
    div Styles.historyViewStyles
        (List.map
            (\general ->
                div Styles.historyViewRowStyles
                    [ button
                        (Styles.historyViewDeleteStyles
                            ++ [ onClick (Types.RemoveItem general) ]
                        )
                        [ text "Remover" ]
                    , button
                        (Styles.historyViewItemStyles
                            ++ [ onClick
                                    (Types.SetCurrent general)
                               ]
                        )
                        [ text (label general) ]
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
        , viewCurrentModel model
        ]


viewCurrentModel : Types.Model -> Html Types.Msg
viewCurrentModel model =
    case model.currentAutomaton of
        Err msg ->
            h1 [] [ text msg ]

        Ok (Models.Automaton automaton) ->
            VAutomata.viewCurrentAutomaton automaton

        Ok (Models.Grammar grammar) ->
            h1 [] [ text "Not yet implemented" ]



-- Defines the view of the sentence input area


viewSentenceInput : Types.Model -> Html Types.Msg
viewSentenceInput model =
    case model.currentAutomaton of
        Err _ ->
            text ""

        Ok (Models.Automaton automaton) ->
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

        otherwise ->
            text ""



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
        Ok (Models.Automaton (Automata.FiniteNonDeterministic _)) ->
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
        Ok (Models.Automaton (Automata.FiniteDeterministic _)) ->
            button (onClick Types.Minimize :: Styles.rightPanelButtonStyles)
                [ text "Minimizar AFD" ]

        otherwise ->
            text ""
