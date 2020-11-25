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
import Models.Grammars as Grammars
import Models.Models as Models
import Operations.Basics as OpBasics
import Operations.Conversion.Automata as CAutomata
import Operations.Conversion.Grammars as CGrammars
import Operations.GLC as OpGLC
import Operations.Minimization as OpMin
import Operations.SentenceValidation.Automata as Validation
import Operations.SentenceValidation.GLC as GLCValidation
import Parsing.Automata as PAutomata
import Parsing.Grammars as PGrammars
import Parsing.Regex as PRegex
import Saving.Automata as SAutomata
import Saving.Grammars as SGrammars
import Saving.Regex as SRegex
import Types.Types as Types
import Utils.Utils as Utils
import View.Automata as VAutomata
import View.Grammars as VGrammars
import View.Regex as VRegex
import View.Styles as Styles



{- The main view function. Defines a title and the viewing areas -}


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



{- Defines the look of the left panel -}


viewLeftPanel : Types.Model -> Html Types.Msg
viewLeftPanel model =
    div Styles.leftPanelStyles
        [ h3 Styles.currentAutomatonTitleStyles [ text "Histórico" ]
        , historyView model
        ]



{- Defines the look of the history -}


historyView : Types.Model -> Html Types.Msg
historyView model =
    let
        label general =
            case general of
                Models.Automaton (Automata.FiniteDeterministic _) ->
                    "AFD"

                Models.Automaton (Automata.FiniteNonDeterministic _) ->
                    "AFND"

                Models.Grammar (Grammars.Regular _) ->
                    "GR"

                Models.Grammar (Grammars.ContextFree _) ->
                    "GLC"

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



{- Defines the view of the center panel -}


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



{- View the current model -}


viewCurrentModel : Types.Model -> Html Types.Msg
viewCurrentModel model =
    case model.currentItem of
        Err msg ->
            h1 [] [ text msg ]

        Ok (Models.Automaton automaton) ->
            VAutomata.viewCurrentAutomaton automaton

        Ok (Models.Grammar grammar) ->
            VGrammars.viewCurrentGrammar grammar

        Ok (Models.Regex idRegexes) ->
            VRegex.viewIdRegexes idRegexes



{- Defines the view of the sentence input area -}


viewSentenceInput : Types.Model -> Html Types.Msg
viewSentenceInput model =
    case model.currentItem of
        Err _ ->
            text ""

        Ok (Models.Automaton automaton) ->
            viewSentenceInputHelp (Validation.validateSentence automaton)
                model.currentSentence

        Ok (Models.Grammar (Grammars.ContextFree glc)) ->
            viewSentenceInputHelp (GLCValidation.validateSentence glc)
                model.currentSentence

        _ ->
            text ""


viewSentenceInputHelp :
    (String -> Result String Bool)
    -> String
    -> Html Types.Msg
viewSentenceInputHelp f sentence =
    let
        validated =
            f sentence
    in
    div Styles.sentenceInputStyles
        [ input
            (Styles.sentenceInputStyles
                ++ [ placeholder "Insira uma sentença para ser validada"
                   , value sentence
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



{- Defines the view of the right panel -}


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
                :: loadButton "autômato finito não determinístico"
                    (Types.FileRequested
                        (PAutomata.parseAFND
                            >> Maybe.map
                                (Automata.FiniteNonDeterministic
                                    >> Models.Automaton
                                )
                        )
                    )
                :: loadButton "gramática regular"
                    (Types.FileRequested
                        (PGrammars.parseGR
                            >> Maybe.map (Grammars.Regular >> Models.Grammar)
                        )
                    )
                :: loadButton "gramática livre de contexto"
                    (Types.FileRequested
                        (PGrammars.parseGLC
                            >> Maybe.map
                                (Grammars.ContextFree >> Models.Grammar)
                        )
                    )
                :: loadButton "expressão regular"
                    (Types.FileRequested
                        (PRegex.parseRegex >> Maybe.map Models.Regex)
                    )
                :: List.filterMap (\f -> f model)
                    [ saveButton
                    , afndToAfdButton
                    , complementButton
                    , minimizeButton
                    , unionButton
                    , intersectionButton
                    , grToAfndButton
                    , afdToGrButton
                    , erToAfdButton
                    , removeEpsilonButton
                    , removeLeftRecursionButton
                    , removeNonDeterminismButton
                    , removeUselessButton
                    , toChomskyButton
                    ]
            )
        ]



{- Button to load a new model -}


loadButton : String -> Types.Msg -> Html Types.Msg
loadButton caption msg =
    button (onClick msg :: Styles.rightPanelButtonStyles)
        [ text ("Carregar " ++ caption) ]



{- Convert Maybe Html to Html -}


maybeHtmlToHtml : Maybe (Html a) -> Html a
maybeHtmlToHtml =
    Maybe.withDefault (text "")



{- Button to save the current model -}


saveButton : Types.Model -> Maybe (Html Types.Msg)
saveButton model =
    let
        ( name, title, content ) =
            case model.currentItem of
                Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
                    ( "afd"
                    , "autômato finito determinístico"
                    , SAutomata.deterministicAutomatonToString afd
                    )

                Ok (Models.Automaton (Automata.FiniteNonDeterministic afnd)) ->
                    ( "afnd"
                    , "autômato finito não determinístico"
                    , SAutomata.nonDeterministicAutomatonToString afnd
                    )

                Ok (Models.Grammar (Grammars.Regular gr)) ->
                    ( "gr"
                    , "gramática regular"
                    , SGrammars.regularGrammarToString gr
                    )

                Ok (Models.Grammar (Grammars.ContextFree glc)) ->
                    ( "glc"
                    , "gramática livre de contexto"
                    , SGrammars.contextFreeGrammarToString glc
                    )

                Ok (Models.Regex regexes) ->
                    ( "er"
                    , "expressão regular"
                    , String.join "\n" <|
                        List.map SRegex.idRegexToString regexes
                    )

                _ ->
                    ( "", "", "" )
    in
    case model.currentItem of
        Ok _ ->
            button
                (onClick (Types.SaveFile (name ++ ".txt") "text/txt" content)
                    :: Styles.rightPanelButtonStyles
                )
                [ text ("Salvar " ++ title) ]
                |> Just

        _ ->
            Nothing



{- Button to do union -}


unionButton : Types.Model -> Maybe (Html Types.Msg)
unionButton model =
    if Utils.firstTwoAreAFDs model.itemHistory then
        Just
            (button (onClick Types.DoUnion :: Styles.rightPanelButtonStyles)
                [ text "Fazer união nos últimos dois autômatos" ]
            )

    else
        Nothing



{- Button to do intersection -}


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



{- Button to convert AFND into AFD -}


afndToAfdButton : Types.Model -> Maybe (Html Types.Msg)
afndToAfdButton model =
    case model.currentItem of
        Ok (Models.Automaton (Automata.FiniteNonDeterministic afnd)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Automaton (Automata.FiniteNonDeterministic a) ->
                                    CAutomata.afndToAfd a
                                        |> Automata.FiniteDeterministic
                                        |> Models.Automaton
                                        |> Ok

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Converter AFND para AFD" ]
                |> Just

        _ ->
            Nothing



{- Button to do complement -}


complementButton : Types.Model -> Maybe (Html Types.Msg)
complementButton model =
    case model.currentItem of
        Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
            Just
                (button
                    (onClick
                        (Types.SetWithFunction
                            (\m ->
                                case m of
                                    Models.Automaton (Automata.FiniteDeterministic a) ->
                                        OpBasics.complement a
                                            |> Automata.FiniteDeterministic
                                            |> Models.Automaton
                                            |> Ok

                                    _ ->
                                        Ok m
                            )
                        )
                        :: Styles.rightPanelButtonStyles
                    )
                    [ text "Fazer complemento" ]
                )

        _ ->
            Nothing



{- Button to minimize -}


minimizeButton : Types.Model -> Maybe (Html Types.Msg)
minimizeButton model =
    case model.currentItem of
        Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Automaton (Automata.FiniteDeterministic a) ->
                                    OpMin.minimizeAFD a
                                        |> Automata.FiniteDeterministic
                                        |> Models.Automaton
                                        |> Ok

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Minimizar AFD" ]
                |> Just

        _ ->
            Nothing



{- Button to convert a GR to an AFND -}


grToAfndButton : Types.Model -> Maybe (Html Types.Msg)
grToAfndButton model =
    case model.currentItem of
        Ok (Models.Grammar (Grammars.Regular grammar)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Grammar (Grammars.Regular g) ->
                                    CGrammars.grToAfnd g
                                        |> Automata.FiniteNonDeterministic
                                        |> Models.Automaton
                                        |> Ok

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Converter para AFND" ]
                |> Just

        _ ->
            Nothing



{- Button to convert an AFD to a GR -}


afdToGrButton : Types.Model -> Maybe (Html Types.Msg)
afdToGrButton model =
    case model.currentItem of
        Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Automaton (Automata.FiniteDeterministic a) ->
                                    CAutomata.afdToGr a
                                        |> Grammars.Regular
                                        |> Models.Grammar
                                        |> Ok

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Converter para GR" ]
                |> Just

        _ ->
            Nothing



{- Button to convert an ER to an AFD -}


erToAfdButton : Types.Model -> Maybe (Html Types.Msg)
erToAfdButton model =
    case model.currentItem of
        Ok (Models.Regex _) ->
            button (onClick Types.ConvertERToAFD :: Styles.rightPanelButtonStyles)
                [ text "Converter para AFD" ]
                |> Just

        _ ->
            Nothing



{- Button to remove epsilon -}


removeEpsilonButton : Types.Model -> Maybe (Html Types.Msg)
removeEpsilonButton model =
    case model.currentItem of
        Ok (Models.Grammar (Grammars.ContextFree glc)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Grammar (Grammars.ContextFree g) ->
                                    OpGLC.removeEpsilon g
                                        |> Grammars.ContextFree
                                        |> Models.Grammar
                                        |> Ok

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Remover Epsilon" ]
                |> Just

        _ ->
            Nothing



{- Button to remove left recursion -}


removeLeftRecursionButton : Types.Model -> Maybe (Html Types.Msg)
removeLeftRecursionButton model =
    case model.currentItem of
        Ok (Models.Grammar (Grammars.ContextFree glc)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Grammar (Grammars.ContextFree g) ->
                                    case OpGLC.eliminateLeftRecursion g of
                                        Ok newGlc ->
                                            newGlc
                                                |> Grammars.ContextFree
                                                |> Models.Grammar
                                                |> Ok

                                        Err x ->
                                            Err x

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Remover recursão à esquerda" ]
                |> Just

        _ ->
            Nothing



{- Button to remove non determinism -}


removeNonDeterminismButton : Types.Model -> Maybe (Html Types.Msg)
removeNonDeterminismButton model =
    case model.currentItem of
        Ok (Models.Grammar (Grammars.ContextFree glc)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Grammar (Grammars.ContextFree g) ->
                                    case OpGLC.factorGLC g of
                                        Ok newGlc ->
                                            newGlc
                                                |> Grammars.ContextFree
                                                |> Models.Grammar
                                                |> Ok

                                        Err x ->
                                            Err x

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Fatorar/Remover não determinismo" ]
                |> Just

        _ ->
            Nothing



{- Button to remove useless symbols -}


removeUselessButton : Types.Model -> Maybe (Html Types.Msg)
removeUselessButton model =
    case model.currentItem of
        Ok (Models.Grammar (Grammars.ContextFree glc)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Grammar (Grammars.ContextFree g) ->
                                    OpGLC.removeUseless g
                                        |> Grammars.ContextFree
                                        |> Models.Grammar
                                        |> Ok

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Remover símbolos inúteis" ]
                |> Just

        _ ->
            Nothing



{- Button to transform a GLC into the normal Chomsky form -}


toChomskyButton : Types.Model -> Maybe (Html Types.Msg)
toChomskyButton model =
    case model.currentItem of
        Ok (Models.Grammar (Grammars.ContextFree glc)) ->
            button
                (onClick
                    (Types.SetWithFunction
                        (\m ->
                            case m of
                                Models.Grammar (Grammars.ContextFree g) ->
                                    OpGLC.transformToChomsky g
                                        |> Grammars.ContextFree
                                        |> Models.Grammar
                                        |> Ok

                                _ ->
                                    Ok m
                        )
                    )
                    :: Styles.rightPanelButtonStyles
                )
                [ text "Transformar para forma normal de Chomsky" ]
                |> Just

        _ ->
            Nothing
