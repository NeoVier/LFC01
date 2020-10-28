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
import File exposing (File)
import File.Select as Select
import Models.Automata as Automata
import Models.Models as Models
import Operations.Basics as BasicOperations
import Operations.Minimization as Minimization
import Parsing.Automata as PAutomata
import Parsing.Grammars as PGrammars
import Task
import Types.Types as Types
import Utils.Utils as Utils
import View.View as View



-- MAIN
-- The program entrypoint, which defines the browser element according to The
-- Elm Architecture


main : Program () Types.Model Types.Msg
main =
    Browser.element
        { init = Types.init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE
-- The main update function, which gets called whenever a Types.Msg is fired


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update msg model =
    case msg of
        Types.AFDRequested ->
            ( model, Select.file [ "text/txt" ] Types.AFDSelected )

        Types.AFDSelected file ->
            ( model, Task.perform Types.AFDLoaded (File.toString file) )

        Types.AFDLoaded content ->
            case PAutomata.parseAFD content of
                Nothing ->
                    ( { model | currentItem = Err "Erro ao ler o autômato" }
                    , Cmd.none
                    )

                Just automaton ->
                    ( { model
                        | currentItem =
                            Ok (Models.Automaton (Automata.FiniteDeterministic automaton))
                        , itemHistory =
                            Models.Automaton
                                (Automata.FiniteDeterministic automaton)
                                :: model.itemHistory
                      }
                    , Cmd.none
                    )

        Types.AFNDRequested ->
            ( model, Select.file [ "text/txt" ] Types.AFNDSelected )

        Types.AFNDSelected file ->
            ( model, Task.perform Types.AFNDLoaded (File.toString file) )

        Types.AFNDLoaded content ->
            case PAutomata.parseAFND content of
                Nothing ->
                    ( { model
                        | currentItem = Err "Erro ao ler o autômato"
                      }
                    , Cmd.none
                    )

                Just automaton ->
                    ( { model
                        | currentItem =
                            Ok
                                (Automata.FiniteNonDeterministic
                                    automaton
                                    |> Models.Automaton
                                )
                        , itemHistory =
                            Models.Automaton
                                (Automata.FiniteNonDeterministic automaton)
                                :: model.itemHistory
                      }
                    , Cmd.none
                    )

        Types.GRRequested ->
            ( model, Select.file [ "text/txt" ] Types.GRSelected )

        Types.GRSelected file ->
            ( model, Task.perform Types.GRLoaded (File.toString file) )

        Types.GRLoaded content ->
            case PGrammars.parseGR content of
                Nothing ->
                    ( { model
                        | currentItem = Err "Erro ao ler a gramática"
                      }
                    , Cmd.none
                    )

                Just grammar ->
                    ( { model
                        | currentItem = Ok (Models.Grammar grammar)
                        , itemHistory =
                            Models.Grammar grammar
                                :: model.itemHistory
                      }
                    , Cmd.none
                    )

        Types.ConvertAFNDToAFD ->
            case model.currentItem of
                Ok (Models.Automaton (Automata.FiniteNonDeterministic afnd)) ->
                    let
                        converted =
                            Automata.FiniteDeterministic
                                (CAutomata.afndToAfd
                                    afnd
                                )
                                |> Models.Automaton
                    in
                    ( { model
                        | currentItem =
                            Ok
                                converted
                        , itemHistory = converted :: model.itemHistory
                      }
                    , Cmd.none
                    )

                otherwise ->
                    ( model, Cmd.none )

        Types.SetCurrent general ->
            ( { model | currentItem = Ok general }
            , Cmd.none
            )

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

        Types.DoComplement ->
            case model.currentItem of
                Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
                    let
                        result =
                            BasicOperations.complement afd
                                |> Automata.FiniteDeterministic
                                |> Models.Automaton
                    in
                    ( { model
                        | currentItem = Ok result
                        , itemHistory = result :: model.itemHistory
                      }
                    , Cmd.none
                    )

                otherwise ->
                    ( model, Cmd.none )

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

        Types.Minimize ->
            case model.currentItem of
                Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
                    let
                        result =
                            Minimization.minimizeAFD afd
                                |> Automata.FiniteDeterministic
                                |> Models.Automaton
                    in
                    ( { model
                        | currentItem =
                            Ok result
                        , itemHistory = result :: model.itemHistory
                      }
                    , Cmd.none
                    )

                otherwise ->
                    ( model, Cmd.none )

        Types.ConvertGRToAFND ->
            case model.currentItem of
                Ok (Models.Grammar grammar) ->
                    let
                        result =
                            CGrammars.grToAfd grammar
                                |> Automata.FiniteNonDeterministic
                                |> Models.Automaton
                    in
                    ( { model
                        | currentItem = Ok result
                        , itemHistory = result :: model.itemHistory
                      }
                    , Cmd.none
                    )

                otherwise ->
                    ( model, Cmd.none )

        Types.ConvertAFDToGR ->
            case model.currentItem of
                Ok (Models.Automaton (Automata.FiniteDeterministic afd)) ->
                    let
                        result =
                            CAutomata.afdToGr afd
                                |> Models.Grammar
                    in
                    ( { model
                        | currentItem = Ok result
                        , itemHistory = result :: model.itemHistory
                      }
                    , Cmd.none
                    )

                otherwise ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS
-- We don't need any subscriptions for this application


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.none
