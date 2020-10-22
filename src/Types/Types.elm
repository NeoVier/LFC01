module Types.Types exposing (..)

import File exposing (File)
import Models.Automata as Automata
import Tests.Automata



-- MODEL


type alias Model =
    { afds : List Automata.AFD
    , currentAutomaton : Maybe Automata.Automaton
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ Tests.Automata.afdTest0 ]
        (Just
            (Automata.FiniteDeterministic Tests.Automata.afdTest0)
        )
    , Cmd.none
    )



-- MESSAGE


type Msg
    = AFDRequested
    | AFDSelected File
    | AFDLoaded String
