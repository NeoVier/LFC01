module Types.Types exposing (..)

import File exposing (File)
import Models.Automata as Automata
import Utils.Utils exposing (filterMaybe)



-- MODEL


type alias Model =
    { automataHistory : List Automata.Automaton
    , currentAutomaton : Result String Automata.Automaton
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
        (Err
            "Nenhum autômato carregado"
        )
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
