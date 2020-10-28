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
import File exposing (File)
import File.Select as Select
import Models.Automata as Automata
import Parsing.Automata as PAutomata
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
                    ( { model | currentAutomaton = Err "Erro ao ler o autômato" }
                    , Cmd.none
                    )

                Just automaton ->
                    ( { model
                        | currentAutomaton =
                            Ok (Automata.FiniteDeterministic automaton)
                        , automataHistory =
                            Automata.FiniteDeterministic
                                automaton
                                :: model.automataHistory
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
                        | currentAutomaton = Err "Erro ao ler o autômato"
                      }
                    , Cmd.none
                    )

                Just automaton ->
                    ( { model
                        | currentAutomaton =
                            Ok
                                (Automata.FiniteNonDeterministic
                                    automaton
                                )
                        , automataHistory =
                            Automata.FiniteNonDeterministic automaton
                                :: model.automataHistory
                      }
                    , Cmd.none
                    )

        Types.ConvertAFNDToAFD ->
            case model.currentAutomaton of
                Ok (Automata.FiniteNonDeterministic afnd) ->
                    let
                        converted =
                            Automata.FiniteDeterministic
                                (CAutomata.afndToAfd
                                    afnd
                                )
                    in
                    ( { model
                        | currentAutomaton =
                            Ok
                                converted
                        , automataHistory = converted :: model.automataHistory
                      }
                    , Cmd.none
                    )

                otherwise ->
                    ( model, Cmd.none )

        Types.SetAutomaton automaton ->
            ( { model | currentAutomaton = Ok automaton }
            , Cmd.none
            )

        Types.SetSentence sentence ->
            ( { model | currentSentence = sentence }, Cmd.none )



-- SUBSCRIPTIONS
-- We don't need any subscriptions for this application


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.none
