module Types.Types exposing (..)

import File exposing (File)
import Models.Automata as Automata
import Models.Models as Models
import Utils.Utils exposing (filterMaybe)



-- MODEL


type alias Model =
    { automataHistory : List Models.General
    , currentAutomaton : Result String Models.General
    , currentSentence : String
    }


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
