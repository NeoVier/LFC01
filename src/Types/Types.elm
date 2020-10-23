module Types.Types exposing (..)

import File exposing (File)
import Models.Automata as Automata
import Utils.Utils exposing (filterMaybe)



-- MODEL


type alias Model =
    { afds : List Automata.AFD
    , currentAutomaton : Result String Automata.Automaton
    }



-- init _ =
--     ( Model []
--         (Err
--             "Nenhum autômato carregado"
--         )
--     , Cmd.none
--     )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] (Ok (Automata.FiniteNonDeterministic Automata.afnd0)), Cmd.none )



-- MESSAGE


type Msg
    = AFDRequested
    | AFDSelected File
    | AFDLoaded String
    | AFNDRequested
    | AFNDSelected File
    | AFNDLoaded String
