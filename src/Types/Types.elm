{-
   Types/Types.elm
   Author: Henrique da Cunha Buss
   Creation: October/2020
   This file contains definitions of the types used for the Elm Architecture
-}


module Types.Types exposing (..)

import File exposing (File)
import Models.Automata as Automata
import Models.Models as Models
import Utils.Utils exposing (filterMaybe)



-- MODEL
-- A model has a current automaton, a history, and a sentence


type alias Model =
    { automataHistory : List Models.General
    , currentAutomaton : Result String Models.General
    , currentSentence : String
    }



-- Initial model - Starts with no history, no automaton and no sentence


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
        (Err
            "Nenhum autômato carregado"
        )
        ""
    , Cmd.none
    )



-- MESSAGE
-- Messages can be:
-- xRequested -> the user requested to select a File
-- xSelected File -> a File was selected to be parsed
-- xLoaded String -> a String was loaded from a File
-- ConvertAFNDToAFD -> the user requested to convert the current AFND to a AFD
-- SetAutomaton Automaton -> the user requested to set the current automaton
-- SetSentence String -> the user requested to set the current sentence


type Msg
    = AFDRequested
    | AFDSelected File
    | AFDLoaded String
    | AFNDRequested
    | AFNDSelected File
    | AFNDLoaded String
    | ConvertAFNDToAFD
    | SetAutomaton Automata.Automaton
    | SetSentence String
