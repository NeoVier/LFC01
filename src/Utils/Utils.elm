module Utils.Utils exposing (..)

import List exposing (concatMap, filter, foldr, map, member, sortWith)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition


normalizeTransitionsDeterministic : Automata.AFD -> Automata.AFD
normalizeTransitionsDeterministic afd =
    let
        symbolsUsed =
            concatMap (\transition -> transition.conditions) afd.transitions

        missing =
            filter (\symbol -> not (member symbol symbolsUsed)) afd.alphabet.symbols
    in
    afd



-- normalizeTransitionDeterministic :
--     Transition.DeterministicTransition
--     -> Alphabet.Alphabet
--     -> Transition.DeterministicTransition
-- normalizeTransitionDeterministic t a =
--     let
--         missing =
--             filter (\symbol -> not (member symbol t.conditions)) a.symbols
--     in
--     t


getOutTransitionsDeterministic :
    Automata.AFD
    -> State.State
    -> List Transition.DeterministicTransition
getOutTransitionsDeterministic afd prevState =
    let
        existing =
            filter (\transition -> transition.prevState == prevState)
                afd.transitions

        existingSymbols =
            concatMap (\transition -> transition.conditions) existing

        missingSymbols =
            filter (\symbol -> not (member symbol existingSymbols))
                afd.alphabet.symbols
    in
    Transition.DeterministicTransition prevState State.Dead missingSymbols
        :: existing


getFlatTransitionDeterministic :
    Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
getFlatTransitionDeterministic transition =
    map
        (\condition -> { transition | conditions = [ condition ] })
        transition.conditions


getFlatOutTransitionsDeterministic :
    Automata.AFD
    -> State.State
    -> List Transition.DeterministicTransition
getFlatOutTransitionsDeterministic afd state =
    getOutTransitionsDeterministic afd state
        |> concatMap getFlatTransitionDeterministic


compareTransitions :
    Transition.DeterministicTransition
    -> Transition.DeterministicTransition
    -> Order
compareTransitions a b =
    compare a.conditions b.conditions


sortTransitionsDeterministic :
    List Transition.DeterministicTransition
    -> List Transition.DeterministicTransition
sortTransitionsDeterministic =
    sortWith compareTransitions
