module View.View exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Types.Types as Types
import View.Automata.Automata as VAutomata
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
            [ div Styles.leftPanelStyles
                [ h3 Styles.currentAutomatonTitleStyles [ text "Histórico" ]
                , historyView model
                ]
            , div Styles.currentAutomatonStyles
                [ h3 Styles.currentAutomatonTitleStyles
                    [ text "Autômato atual" ]
                , VAutomata.viewCurrentAutomaton model
                ]
            , div Styles.rightPanelStyles
                [ h3 Styles.currentAutomatonTitleStyles [ text "Controles" ]
                , button
                    (onClick Types.AFDRequested
                        :: Styles.rightPanelButtonStyles
                    )
                    [ text "Carregar autômato finito" ]
                ]
            ]
        ]


historyView : Types.Model -> Html Types.Msg
historyView model =
    ul Styles.historyViewStyles
        (List.map
            (\x ->
                li Styles.historyViewItemStyles
                    [ text "AFD" ]
            )
            model.afds
        )
