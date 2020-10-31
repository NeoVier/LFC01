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
-- Each message is a communication between the user and the program, according
-- to the Elm Architecture


type Msg
    = FileRequested (String -> Maybe Models.General)
    | FileSelected (String -> Maybe Models.General) File
    | FileLoaded (String -> Maybe Models.General) String
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
