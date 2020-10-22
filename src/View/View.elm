module View.View exposing (view)

import Html exposing (..)
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
            [ div Styles.currentAutomatonStyles
                [ h3 Styles.currentAutomatonTitleStyles
                    [ text "Autômato atual" ]
                , VAutomata.viewCurrentAutomaton model
                ]
            ]
        ]
