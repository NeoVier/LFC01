module View.Automata exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition
import Types.Types as Types exposing (Msg)
import Utils.Utils as Utils
import View.Automata.Deterministic as VAFD
import View.Styles exposing (..)


viewCurrentAutomaton : Types.Model -> Html msg
viewCurrentAutomaton model =
    case model.currentAutomaton of
        Err message ->
            h1 [] [ text message ]

        Ok automaton ->
            case automaton of
                Automata.FiniteDeterministic afd ->
                    VAFD.viewAFD afd

                Automata.FiniteNonDeterministic afnd ->
                    viewAFND afnd


viewAFND : Automata.AFND -> Html msg
viewAFND afnd =
    text ""



-- case afnd.alphabet of
--     Alphabet.NDA symbols epsilon ->
--         table tableStyles
--             (viewAutomataHeader symbols
--                 ++ [ "&" ]
--                 :: getAutomatonRows (Automata.FiniteNonDeterministic afnd)
--             )


viewNonDeterministicTransition :
    Transition.NonDeterministicTransition
    -> Html msg
viewNonDeterministicTransition transition =
    td tableItemStyles
        (List.map
            (\next ->
                case next of
                    State.Dead ->
                        text "-"

                    State.Valid nextLabel ->
                        text (nextLabel ++ " ")
            )
            transition.nextStates
        )
