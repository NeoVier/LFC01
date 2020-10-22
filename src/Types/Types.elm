module Types.Types exposing (..)

import File exposing (File)
import Models.Automata as Automata



-- MODEL


type alias Model =
    { afds : List Automata.AFD
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
