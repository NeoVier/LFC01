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
import Parsing.Automata as PAutomata
import Parsing.Grammars as PGrammars
import Parsing.Regex as PRegex
import Types.Types as Types
import Utils.Utils as Utils
import View.Automata as VAutomata
import View.Grammars.Regular as VGrammars
import View.Regex as VRegex
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

                Models.Grammar _ ->
                    "GR"

                Models.Regex _ ->
                    "ER"
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
            model.itemHistory
        )



-- Defines the view of the center panel


viewCenterPanel : Types.Model -> Html Types.Msg
viewCenterPanel model =
    let
        title =
            case model.currentItem of
                Err _ ->
                    ""

                Ok (Models.Automaton _) ->
                    "Autômato atual"

                Ok (Models.Grammar _) ->
                    "Gramática atual"

                Ok (Models.Regex _) ->
                    "Expressão regular atual"
    in
    div Styles.currentAutomatonStyles
        [ h3 Styles.currentAutomatonTitleStyles
            [ text title ]
        , viewSentenceInput model
        , viewCurrentModel model
        ]


viewCurrentModel : Types.Model -> Html Types.Msg
viewCurrentModel model =
    case model.currentItem of
        Err msg ->
            h1 [] [ text msg ]

        Ok (Models.Automaton automaton) ->
            VAutomata.viewCurrentAutomaton automaton

        Ok (Models.Grammar grammar) ->
            VGrammars.viewGR grammar

        Ok (Models.Regex idRegexes) ->
            VRegex.viewIdRegexes idRegexes



-- Defines the view of the sentence input area


viewSentenceInput : Types.Model -> Html Types.Msg
viewSentenceInput model =
    case model.currentItem of
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

        _ ->
            text ""



-- Defines the view of the right panel


viewRightPanel : Types.Model -> Html Types.Msg
viewRightPanel model =
    div Styles.rightPanelStyles
        [ h3 Styles.currentAutomatonTitleStyles
            [ text "Controles" ]
        , div
            Styles.rightPanelControlContainerStyles
            (loadButton "autômato finito determinístico"
                (Types.FileRequested
                    (PAutomata.parseAFD
                        >> Maybe.map Automata.FiniteDeterministic
                        >> Maybe.map Models.Automaton
                    )
                )
                :: loadButton "autômato finito não-determinístico"
                    (Types.FileRequested
                        (PAutomata.parseAFND
                            >> Maybe.map Automata.FiniteNonDeterministic
                            >> Maybe.map Models.Automaton
                        )
                    )
                :: loadButton "gramática regular"
                    (Types.FileRequested
                        (PGrammars.parseGR
                            >> Maybe.map Models.Grammar
                        )
                    )
                :: loadButton "expressão regular"
                    (Types.FileRequested
                        (PRegex.parseRegex
                            >> Maybe.map Models.Regex
                        )
                    )
                :: List.map (\f -> f model |> maybeHtmlToHtml)
                    [ convertButton
                    , complementButton
                    , minimizeButton
                    , unionButton
                    , intersectionButton
                    , grToAfdButton
                    , afdToGrButton
                    , erToAfdButton
                    ]
            )
        ]


loadButton : String -> Types.Msg -> Html Types.Msg
loadButton caption msg =
    button (onClick msg :: Styles.rightPanelButtonStyles)
        [ text ("Carregar " ++ caption) ]


maybeHtmlToHtml : Maybe (Html a) -> Html a
maybeHtmlToHtml html =
    case html of
        Nothing ->
            text ""

        Just valid ->
            valid


convertButton : Types.Model -> Maybe (Html Types.Msg)
convertButton model =
    case model.currentItem of
        Ok (Models.Automaton (Automata.FiniteNonDeterministic _)) ->
            button
                (onClick Types.ConvertAFNDToAFD
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Converter AFND para AFD" ]
                |> Just

        _ ->
            Nothing


unionButton : Types.Model -> Maybe (Html Types.Msg)
unionButton model =
    if Utils.firstTwoAreAFDs model.itemHistory then
        Just
            (button (onClick Types.DoUnion :: Styles.rightPanelButtonStyles)
                [ text "Fazer união nos últimos dois autômatos" ]
            )

    else
        Nothing


complementButton : Types.Model -> Maybe (Html Types.Msg)
complementButton model =
    case model.currentItem of
        Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
            Just
                (button
                    (onClick Types.DoComplement
                        :: Styles.rightPanelButtonStyles
                    )
                    [ text "Fazer complemento" ]
                )

        _ ->
            Nothing


intersectionButton : Types.Model -> Maybe (Html Types.Msg)
intersectionButton model =
    if Utils.firstTwoAreAFDs model.itemHistory then
        Just
            (button
                (onClick Types.DoIntersection
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Fazer interseção nos últimos dois autômatos" ]
            )

    else
        Nothing


minimizeButton : Types.Model -> Maybe (Html Types.Msg)
minimizeButton model =
    case model.currentItem of
        Ok (Models.Automaton (Automata.FiniteDeterministic _)) ->
            button (onClick Types.Minimize :: Styles.rightPanelButtonStyles)
                [ text "Minimizar AFD" ]
                |> Just

        _ ->
            Nothing


grToAfdButton : Types.Model -> Maybe (Html Types.Msg)
grToAfdButton model =
    case model.currentItem of
        Ok (Models.Grammar grammar) ->
            button
                (onClick Types.ConvertGRToAFND
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Converter para AFND" ]
                |> Just

        _ ->
            Nothing


afdToGrButton : Types.Model -> Maybe (Html Types.Msg)
afdToGrButton model =
    case model.currentItem of
        Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
            button (onClick Types.ConvertAFDToGR :: Styles.rightPanelButtonStyles)
                [ text "Converter para GR" ]
                |> Just

        _ ->
            Nothing


erToAfdButton : Types.Model -> Maybe (Html Types.Msg)
erToAfdButton model =
    case model.currentItem of
        Ok (Models.Regex _) ->
            button (onClick Types.ConvertERToAFD :: Styles.rightPanelButtonStyles)
                [ text "Converter para AFD" ]
                |> Just

        _ ->
            Nothing
