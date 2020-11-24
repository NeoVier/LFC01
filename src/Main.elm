{-
   Main.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains the main and update functions, and is the entrypoint
   of the application
-}


module Main exposing (..)

import Browser
import Conversion.Automata as CAutomata
import Conversion.Grammars as CGrammars
import Conversion.Regex as CRegex
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Models.Automata as Automata
import Models.Grammars as Grammars
import Models.Models as Models
import Operations.Basics as BasicOperations
import Operations.GLC as OpGLC
import Operations.Minimization as Minimization
import Parsing.Automata as PAutomata
import Parsing.Grammars as PGrammars
import Parsing.Regex as PRegex
import Task
import Types.Types as Types
import Utils.Utils as Utils
import View.View as View



-- MAIN
{- The program entrypoint, which defines the browser element according to The
   Elm Architecture
-}


main : Program () Types.Model Types.Msg
main =
    Browser.element
        { init = Types.init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE
{- The main update function, which gets called whenever a Types.Msg is fired -}


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update msg model =
    case msg of
        Types.FileRequested f ->
            ( model, Select.file [ "text/txt" ] (Types.FileSelected f) )

        Types.FileSelected f file ->
            ( model, Task.perform (Types.FileLoaded f) (File.toString file) )

        Types.FileLoaded f content ->
            case f content of
                Nothing ->
                    ( { model | currentItem = Err "Erro ao ler arquivo" }
                    , Cmd.none
                    )

                Just item ->
                    ( { model
                        | currentItem = Ok item
                        , itemHistory = item :: model.itemHistory
                      }
                    , Cmd.none
                    )

        Types.SaveFile name filetype content ->
            ( model, Download.string name filetype content )

        Types.SetCurrent general ->
            ( { model | currentItem = Ok general }, Cmd.none )

        Types.UpdateCurrent general ->
            ( { model
                | currentItem = Ok general
                , itemHistory =
                    case model.currentItem of
                        Ok curr ->
                            Utils.replaceBy curr general model.itemHistory

                        _ ->
                            model.itemHistory
              }
            , Cmd.none
            )

        Types.SetWithFunction f ->
            case model.currentItem of
                Ok m ->
                    let
                        result =
                            f m

                        newHistory =
                            case result of
                                Ok r ->
                                    if result == model.currentItem then
                                        model.itemHistory

                                    else
                                        r :: model.itemHistory

                                _ ->
                                    model.itemHistory
                    in
                    ( { model
                        | currentItem = result
                        , itemHistory = newHistory
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Types.RemoveItem general ->
            let
                newHistory =
                    List.filter (\a -> a /= general) model.itemHistory
            in
            ( { model
                | itemHistory = newHistory
                , currentItem =
                    case List.head newHistory of
                        Nothing ->
                            Err "Nenhum item carregado"

                        Just a ->
                            Ok a
              }
            , Cmd.none
            )

        Types.SetSentence sentence ->
            ( { model | currentSentence = sentence }, Cmd.none )

        Types.DoUnion ->
            case Utils.getFirstTwoAsAFDs model.itemHistory of
                Nothing ->
                    ( { model
                        | currentItem =
                            Err "Erro ao fazer união"
                      }
                    , Cmd.none
                    )

                Just ( afd1, afd2 ) ->
                    let
                        result =
                            BasicOperations.union afd1 afd2
                                |> Automata.FiniteNonDeterministic
                                |> Models.Automaton
                    in
                    ( { model
                        | currentItem = Ok result
                        , itemHistory = result :: model.itemHistory
                      }
                    , Cmd.none
                    )

        Types.DoIntersection ->
            case Utils.getFirstTwoAsAFDs model.itemHistory of
                Nothing ->
                    ( { model
                        | currentItem = Err "Erro ao fazer interseção"
                      }
                    , Cmd.none
                    )

                Just ( afd1, afd2 ) ->
                    let
                        result =
                            BasicOperations.intersection afd1 afd2
                                |> Automata.FiniteDeterministic
                                |> Models.Automaton
                    in
                    ( { model
                        | currentItem = Ok result
                        , itemHistory = result :: model.itemHistory
                      }
                    , Cmd.none
                    )

        Types.ConvertERToAFD ->
            case model.currentItem of
                Ok (Models.Regex regexes) ->
                    let
                        results =
                            List.map
                                (\( id, regex ) ->
                                    CRegex.erToAfd regex
                                        |> Automata.FiniteDeterministic
                                        |> Models.Automaton
                                )
                                regexes
                    in
                    case List.head results of
                        Nothing ->
                            ( model, Cmd.none )

                        Just afd ->
                            ( { model
                                | currentItem = Ok afd
                                , itemHistory = results ++ model.itemHistory
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        Types.NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS
{- We don't need any subscriptions for this application -}


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.none
