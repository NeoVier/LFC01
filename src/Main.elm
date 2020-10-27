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


main : Program () Types.Model Types.Msg
main =
    Browser.element
        { init = Types.init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


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
                    ( { model
                        | currentAutomaton =
                            Ok
                                (Automata.FiniteDeterministic
                                    (CAutomata.afndToAfd afnd)
                                )
                      }
                    , Cmd.none
                    )

                otherwise ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.none
