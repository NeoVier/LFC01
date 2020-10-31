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
    { itemHistory : List Models.General
    , currentItem : Result String Models.General
    , currentSentence : String
    }



-- Initial model - Starts with no history, no automaton and no sentence


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
        (Err
            "Nenhum item carregado"
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
-- SetCurrent Models.General -> the user requested to set the current automaton
-- RemoveItem Models.General -> the user requested to remove the automaton
-- SetSentence String -> the user requested to set the current sentence
-- DoUnion -> Applies the union operation to the last two automata
-- DoIntersection -> Applies the intersection operation to the last two automata
-- DoComplement -> Applies the complement operation to the current AFD
-- Minimize -> Minimize current automaton
-- ConvertGRToAFND -> Converts current GR to an AFND
-- ConvertAFDToGR -> Converts current AFD to GR


type Msg
    = AFDRequested
    | AFDSelected File
    | AFDLoaded String
    | AFNDRequested
    | AFNDSelected File
    | AFNDLoaded String
    | GRRequested
    | GRSelected File
    | GRLoaded String
    | RegexRequested
    | RegexSelected File
    | RegexLoaded String
    | ConvertAFNDToAFD
    | SetCurrent Models.General
    | RemoveItem Models.General
    | SetSentence String
    | DoUnion
    | DoIntersection
    | DoComplement
    | Minimize
    | ConvertGRToAFND
    | ConvertAFDToGR
    | ConvertERToAFD
